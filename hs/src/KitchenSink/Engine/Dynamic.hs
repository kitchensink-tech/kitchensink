{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Request-time dynamic pages, SQLPage-style: an article whose
@=base:build-info.json@ declares a @route@ (e.g. @{"layout":"dynamic","route":"/users/:id"}@)
is served per-request instead of statically. Its @.sql@ datasets
(@=base:dataset.sql name@) run against a read-only sqlite datasource for
every request, and its @main-content@ (a @.tramaj-doc@ section) is evaluated
per-request against a context extended with @$ctx.request@
(@method@\/@path@\/@params@\/@query@\/@form@\/@cookies@\/@user@).

Both are left unevaluated at load time by "KitchenSink.Engine.SiteLoader"
(see @KitchenSink.Engine.SiteLoader.isDynamicArticle@); this module does the
per-request evaluation.

This is opt-in (@kitchen-sink serve --dynamic@) and, in this first cut,
scoped to what the feature's design notes called out as an acceptable
subset: GET-only (no forms\/POST\/redirects), a single datasource named
@\"main\"@ (no per-dataset datasource selection), and no postgres -- all
recorded as deferred follow-ups, not silently missing.

A page's build-info may set @\"auth\":\"required\"@ to require a caller
identity before any dataset runs (default, and any other value, is
@\"public\"@) -- see "KitchenSink.Engine.Auth" for how identity is
established (HTTP Basic against the @config@ credential provider, backed by
a signed session cookie) and 'DynamicOptions' for how it is wired in from
@kitchen-sink.json@'s @auth@ stanza.
-}
module KitchenSink.Engine.Dynamic (
    DynamicOptions (..),
    noDynamicOptions,
    dynamicOptionsFromConfig,
    dynamicMiddleware,

    -- * exposed for testing/inspection
    DynamicPage (..),
    RouteSegment (..),
    BlobMode (..),
    parseRoute,
    matchRoute,
    extractDynamicPage,
    findDynamicPages,
) where

import Control.Exception (SomeException, bracket, throwIO, try)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LByteString
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3 (ColumnIndex (..), Database, ParamIndex (..), SQLData (..), SQLOpenFlag (..), SQLVFS (..), Statement, StepResult (..))
import Database.SQLite3 qualified as SQLite3
import Network.HTTP.Types (status200, status500)
import Network.Wai qualified as Wai
import Prelude (Double, id, negate, (&&), (+), (-), (<), (>), (>=), (||))

import KitchenSink.Core.Assembler (runAssembler)
import KitchenSink.Core.Assembler.Sections.Json (json)
import KitchenSink.Core.Assembler.Sections.Primitives (getSection, getSections, isBuildInfo, isDataset, isMainContent)
import KitchenSink.Core.Build.Site (Article, Site (..))
import KitchenSink.Core.Build.Target (SourceLocation (..), Sourced (..))
import KitchenSink.Core.Section (BuildInfoData (..), Format (..), Section (..), SectionType (Dataset))
import KitchenSink.Core.Section.Parser (extract)
import KitchenSink.Engine.Auth (AuthPolicy (..), Identity (..), identityFromRequest, parseAuthPolicy, setSessionCookieHeader, unauthorizedResponse)
import KitchenSink.Engine.Config (AuthConfig, Config (..), DatasourceConfig (..))
import KitchenSink.Engine.Templating qualified as Templating
import KitchenSink.Prelude

-- | A route pattern segment: a literal path component, or a @:name@ capture.
data RouteSegment
    = Literal Text
    | Param Text
    deriving (Show, Eq)

-- | How a page's @.sql@ dataset(s) render @BLOB@ columns to JSON.
data BlobMode = BlobBase64 | BlobOmit
    deriving (Show, Eq)

parseBlobMode :: Maybe Text -> BlobMode
parseBlobMode (Just "omit") = BlobOmit
parseBlobMode _ = BlobBase64

defaultRowCap :: Int
defaultRowCap = 1000

-- | Splits a route pattern (@\"\/users\/:id\"@) into matchable segments.
parseRoute :: Text -> [RouteSegment]
parseRoute r = fmap toSeg $ List.filter (not . Text.null) $ Text.splitOn "/" r
  where
    toSeg s
        | Text.isPrefixOf ":" s = Param (Text.drop 1 s)
        | otherwise = Literal s

-- | Matches a parsed route against a request's path segments (as WAI's
-- 'Network.Wai.pathInfo' gives them, already url-decoded). 'Nothing' when
-- the segment count differs or a literal segment doesn't match exactly.
matchRoute :: [RouteSegment] -> [Text] -> Maybe (Map Text Text)
matchRoute pat segs
    | List.length pat /= List.length segs = Nothing
    | List.all literalOk paired = Just (Map.fromList [(name, s) | (Param name, s) <- paired])
    | otherwise = Nothing
  where
    paired = List.zip pat segs
    literalOk (Literal l, s) = l == s
    literalOk (Param _, _) = True

-- | A dynamic page, extracted once per request from its (already-loaded but
-- deliberately unevaluated) 'Article'.
data DynamicPage = DynamicPage
    { dynRoute :: [RouteSegment]
    , dynRowCap :: Int
    , dynBlobMode :: BlobMode
    , dynAuthPolicy :: AuthPolicy
    , dynDatasets :: [(Name, Text)]
    -- ^ dataset name, raw (unevaluated) SQL source
    , dynMainContent :: Text
    -- ^ raw (unevaluated) tramaj-doc source
    , dynSourcePath :: FilePath
    }

-- | Reads a 'DynamicPage' out of an article, iff its build-info declares a
-- @route@. 'Nothing' both for an ordinary article and for a malformed
-- dynamic one (unreadable build-info, or no tramaj-doc main-content section)
-- -- the latter simply never matches any request, same as an unknown layout
-- name elsewhere in this codebase falls back rather than crashing the
-- server.
extractDynamicPage :: forall ext. (Eq ext) => FilePath -> Article ext [Text] -> Maybe DynamicPage
extractDynamicPage path art = do
    binfo <- hush $ runAssembler (extract <$> json @ext @BuildInfoData art isBuildInfo)
    r <- route binfo
    mainSec <- hush $ runAssembler (getSection art isMainContent)
    mainBody <- case mainSec of
        Section _ TramajDoc body -> Just (Text.unlines body)
        _ -> Nothing
    let datasetSecs = either (const []) id $ runAssembler (getSections art isDataset)
    let sqlDatasets =
            [ (name, Text.unlines body)
            | Section (Dataset name) Sql body <- datasetSecs
            ]
    pure
        DynamicPage
            { dynRoute = parseRoute r
            , dynRowCap = maybe defaultRowCap id (rowCap binfo)
            , dynBlobMode = parseBlobMode (blobs binfo)
            , dynAuthPolicy = parseAuthPolicy binfo.auth
            , dynDatasets = sqlDatasets
            , dynMainContent = mainBody
            , dynSourcePath = path
            }

-- | Every dynamic page in a site.
findDynamicPages :: (Eq ext) => Site ext -> [DynamicPage]
findDynamicPages site =
    [ pg
    | Sourced (FileSource path) art <- site.articles
    , Just pg <- [extractDynamicPage path art]
    ]

data DynamicOptions = DynamicOptions
    { dynEnabled :: Bool
    , dynDatasources :: Map Text DatasourceConfig
    , dynAuth :: Maybe AuthConfig
    }

noDynamicOptions :: DynamicOptions
noDynamicOptions = DynamicOptions False Map.empty Nothing

dynamicOptionsFromConfig :: Bool -> Config -> DynamicOptions
dynamicOptionsFromConfig enabled cfg =
    DynamicOptions enabled (maybe Map.empty id cfg.datasources) cfg.auth

{- | Tries every dynamic route (GET only, in this first cut) before falling
back to @app@ (the site's ordinary on-the-fly production). Disabled
entirely -- falls straight to @app@ -- unless 'dynEnabled'.
-}
dynamicMiddleware :: (Eq ext) => DynamicOptions -> IO (Site ext) -> Wai.Application -> Wai.Application
dynamicMiddleware opts readSite fallback req resp
    | not (dynEnabled opts) = fallback req resp
    | Wai.requestMethod req /= "GET" = fallback req resp
    | otherwise = do
        site <- readSite
        case lookupDynamicPage site (Wai.pathInfo req) of
            Nothing -> fallback req resp
            Just (pg, params) -> case Map.lookup "main" (dynDatasources opts) of
                Nothing -> resp $ Wai.responseLBS status500 [("content-type", "text/plain")] "dynamic page requested but no \"main\" datasource is configured (kitchen-sink.json: datasources.main.sqlite)"
                Just dsCfg -> case (dynAuthPolicy pg, dynAuth opts) of
                    (Required, Nothing) ->
                        resp $ Wai.responseLBS status500 [("content-type", "text/plain")] "dynamic page requires auth (\"auth\":\"required\") but kitchen-sink.json has no \"auth\" stanza configured"
                    (policy, mAuthCfg) -> do
                        (mIdent, mFreshCookie) <- case mAuthCfg of
                            Just authCfg -> identityFromRequest authCfg req
                            Nothing -> pure (Nothing, Nothing)
                        if policy == Required && isNothing mIdent
                            then resp unauthorizedResponse
                            else do
                                response <- respondDynamic dsCfg pg params mIdent req
                                resp (attachFreshCookie (Wai.isSecure req) mFreshCookie response)

-- | Adds a @Set-Cookie@ header for a freshly-minted session (see
-- 'KitchenSink.Engine.Auth.identityFromRequest') onto an otherwise-finished
-- response; a no-op when identity came from an already-valid cookie or
-- there is no identity at all.
attachFreshCookie :: Bool -> Maybe Text -> Wai.Response -> Wai.Response
attachFreshCookie _ Nothing response = response
attachFreshCookie secure (Just signedValue) response =
    Wai.mapResponseHeaders (setSessionCookieHeader secure signedValue :) response

lookupDynamicPage :: (Eq ext) => Site ext -> [Text] -> Maybe (DynamicPage, Map Text Text)
lookupDynamicPage site segs =
    List.foldr (\pg acc -> maybe acc (\ps -> Just (pg, ps)) (matchRoute (dynRoute pg) segs)) Nothing (findDynamicPages site)

newtype DynamicPageError = DynamicPageError Text
    deriving (Show)
instance Exception DynamicPageError

respondDynamic :: DatasourceConfig -> DynamicPage -> Map Text Text -> Maybe Identity -> Wai.Request -> IO Wai.Response
respondDynamic dsCfg pg routeParams mIdent req = do
    outcome <- try (renderDynamicPage dsCfg pg routeParams mIdent req)
    pure $ case outcome of
        Right html ->
            Wai.responseLBS status200 [("content-type", "text/html; charset=utf-8")] (LByteString.fromStrict $ Text.encodeUtf8 html)
        Left (e :: SomeException) ->
            Wai.responseLBS status500 [("content-type", "text/plain; charset=utf-8")] (LByteString.fromStrict $ Text.encodeUtf8 $ "dynamic page error: " <> Text.pack (show e))

renderDynamicPage :: DatasourceConfig -> DynamicPage -> Map Text Text -> Maybe Identity -> Wai.Request -> IO Text
renderDynamicPage dsCfg pg routeParams mIdent req =
    bracket (openReadOnly (sqlite dsCfg)) SQLite3.close $ \db -> do
        let bindings = maybe id (\(Identity u) -> Map.insert "user_id" u) mIdent (Map.union routeParams (queryMap req))
        datasetPairs <-
            traverse
                (\(name, sqlText) -> (,) name <$> runDataset db (dynRowCap pg) (dynBlobMode pg) bindings sqlText)
                (dynDatasets pg)
        let datasetsMap = Map.fromList datasetPairs
        let reqCtx = requestContext req routeParams mIdent
        let ctx0 = Templating.buildContext (dynSourcePath pg) 0 "" [] datasetsMap
        let ctx = withRequestContext reqCtx ctx0
        case Templating.evalDocSection Map.empty ctx (dynMainContent pg) of
            Left err -> throwIO $ DynamicPageError (Text.pack (show err))
            Right (_, html) -> pure html

withRequestContext :: Value -> Value -> Value
withRequestContext reqVal (Aeson.Object obj) = Aeson.Object (KeyMap.insert "request" reqVal obj)
withRequestContext _ other = other

requestContext :: Wai.Request -> Map Text Text -> Maybe Identity -> Value
requestContext req params mIdent =
    Aeson.object
        [ ("method", Aeson.toJSON (Text.decodeUtf8 (Wai.requestMethod req)))
        , ("path", Aeson.toJSON (Text.decodeUtf8 (Wai.rawPathInfo req)))
        , ("params", Aeson.toJSON params)
        , ("query", Aeson.toJSON (queryMap req))
        , ("form", Aeson.object []) -- GET-only in this first cut; see module docs
        , ("cookies", Aeson.toJSON (cookieMap req))
        , ("user", maybe Aeson.Null (\(Identity u) -> Aeson.toJSON u) mIdent)
        ]

queryMap :: Wai.Request -> Map Text Text
queryMap req =
    Map.fromList
        [ (Text.decodeUtf8 k, maybe "" Text.decodeUtf8 v)
        | (k, v) <- Wai.queryString req
        ]

cookieMap :: Wai.Request -> Map Text Text
cookieMap req =
    case List.lookup "Cookie" (Wai.requestHeaders req) of
        Nothing -> Map.empty
        Just raw -> Map.fromList $ fmap parseOne $ List.filter (not . Text.null) $ fmap Text.strip $ Text.splitOn ";" (Text.decodeUtf8 raw)
  where
    parseOne kv = let (k, v) = Text.breakOn "=" kv in (k, Text.drop 1 v)

-- | Opens the datasource read-only, with a busy-timeout so a concurrent
-- write from outside kitchen-sink doesn't hang a request forever. Never
-- attempts to set @journal_mode=WAL@: that requires a writable connection,
-- and kitchen-sink never writes to a datasource (bring-your-own db; see the
-- module docs and the feature's design notes). Put the datasource in WAL
-- mode yourself, e.g. from whatever writes to it.
openReadOnly :: FilePath -> IO Database
openReadOnly path = do
    db <- SQLite3.open2 (Text.pack path) [SQLOpenReadOnly] SQLVFSDefault
    SQLite3.exec db "PRAGMA busy_timeout=5000;"
    pure db

-- | Runs one @.sql@ dataset's query, binding every named parameter the
-- query actually references from the (route params ++ query string) map --
-- an unmatched name binds NULL rather than erroring, and a bound value the
-- query never references is silently unused. A statement that isn't a
-- SELECT (e.g. an INSERT) still runs against a read-only connection, so
-- sqlite itself rejects it at `step` time: the caught exception surfaces as
-- the page's error, with no partial HTML written.
runDataset :: Database -> Int -> BlobMode -> Map Text Text -> Text -> IO Value
runDataset db rowCapN blobMode bindings sqlText =
    -- 'bracket', not a plain prepare/finalize pair: a write attempt against
    -- the read-only connection throws mid-'step' (see 'collectRows'), and an
    -- unfinalized statement left dangling on that path would in turn make
    -- the outer connection's own 'SQLite3.close' fail -- masking the real
    -- error behind an unrelated "unfinalized statements" one.
    bracket (SQLite3.prepare db sqlText) SQLite3.finalize $ \stmt -> do
        bindNamedFromMap stmt bindings
        rows <- collectRows stmt rowCapN blobMode
        pure (Aeson.toJSON rows)

bindNamedFromMap :: Statement -> Map Text Text -> IO ()
bindNamedFromMap stmt bindings = do
    ParamIndex n <- SQLite3.bindParameterCount stmt
    traverse_ bindOne [1 .. n]
  where
    bindOne i = do
        mName <- SQLite3.bindParameterName stmt (ParamIndex i)
        case mName >>= stripSigil of
            Just key -> case Map.lookup key bindings of
                Just v -> SQLite3.bindText stmt (ParamIndex i) v
                Nothing -> SQLite3.bindNull stmt (ParamIndex i)
            Nothing -> SQLite3.bindNull stmt (ParamIndex i)

stripSigil :: Text -> Maybe Text
stripSigil t = case Text.uncons t of
    Just (c, rest) | c == ':' || c == '@' || c == '$' -> Just rest
    _ -> Nothing

collectRows :: Statement -> Int -> BlobMode -> IO [Value]
collectRows stmt cap blobMode = go 0
  where
    go n
        | n >= cap = pure []
        | otherwise = do
            r <- SQLite3.step stmt
            case r of
                Done -> pure []
                Row -> do
                    v <- rowValue stmt blobMode
                    (v :) <$> go (n + 1)

rowValue :: Statement -> BlobMode -> IO Value
rowValue stmt blobMode = do
    ColumnIndex count <- SQLite3.columnCount stmt
    let idxs = [ColumnIndex i | i <- [0 .. count - 1]]
    pairs <- traverse (\i -> (,) <$> columnKey stmt i <*> (sqlDataToJson blobMode <$> SQLite3.column stmt i)) idxs
    pure $ Aeson.Object $ KeyMap.fromList [(Key.fromText k, v) | (k, v) <- pairs]

columnKey :: Statement -> ColumnIndex -> IO Text
columnKey stmt i = maybe (Text.pack (show i)) id <$> SQLite3.columnName stmt i

sqlDataToJson :: BlobMode -> SQLData -> Value
sqlDataToJson _ (SQLInteger i) = integerJson i
sqlDataToJson _ (SQLFloat d) = Aeson.toJSON (d :: Double)
sqlDataToJson _ (SQLText t) = Aeson.toJSON t
sqlDataToJson BlobBase64 (SQLBlob b) = Aeson.toJSON (Text.decodeUtf8 (Base64.encode b) :: Text)
sqlDataToJson BlobOmit (SQLBlob _) = Aeson.Null
sqlDataToJson _ SQLNull = Aeson.Null

-- | Integers beyond 2^53 lose precision once round-tripped through a JS (or
-- any IEEE-754-double-backed) JSON consumer, so they are rendered as
-- strings instead, same convention as e.g. protobuf-JSON's int64 mapping.
integerJson :: Int64 -> Value
integerJson i
    | i > maxSafeInteger || i < negate maxSafeInteger = Aeson.toJSON (Text.pack (show i))
    | otherwise = Aeson.toJSON (i :: Int64)
  where
    maxSafeInteger :: Int64
    maxSafeInteger = 9007199254740992 -- 2^53
