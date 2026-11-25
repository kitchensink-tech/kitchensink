{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.OnTheFly where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LByteString
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text.Encoding qualified as Text
import Data.Bool ((&&))
import Data.Typeable (Typeable)
import Network.HTTP.Types (status200, status404)
import Network.Wai as Wai
import Prod.Tracer
import Prometheus qualified as Prometheus

import KitchenSink.Core.Build.Target (ProductionRule (..), Target, destination, destinationUrl, productionRule)
import KitchenSink.Engine.Counters (Counters (..), timeItWithLabel)
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.SiteBuilder
import KitchenSink.Engine.SiteLoader (Site)
import KitchenSink.Engine.Track (DevServerTrack (..), RequestedPath (..), blogTargetTracer, requestedPath)
import KitchenSink.Layout.Blog.Metadata (pathPrefix)
import KitchenSink.Prelude

type TargetPath = ByteString

type FetchTarget ext = RequestedPath -> IO (TargetPath, Maybe (Target ext ()))

findTarget ::
    Engine ext ->
    IO (Site ext) ->
    Prod.Tracer.Tracer IO (DevServerTrack ext) ->
    FetchTarget ext
findTarget engine loadSite track = \origpath -> do
    runTracer track (TargetRequested origpath)
    med <- execLoadMetaExtradata engine
    let rootPath = RequestedPath $ Text.encodeUtf8 (pathPrefix med) <> "/"
    let indexPath = Text.encodeUtf8 (pathPrefix med) <> "/index.html"
    let path = if origpath == rootPath then indexPath else coerce origpath
    tgts <- evalTargets engine med <$> loadSite
    let urlOf tgt = Text.encodeUtf8 (destinationUrl (destination tgt))
    let exact = List.find (\tgt -> urlOf tgt == path) tgts
    let extensionless = do
            htmlPath <- extensionlessHtmlPath path
            tgt <- List.find (\tgt -> isGeneratedPage tgt && urlOf tgt == htmlPath) tgts
            pure (htmlPath, tgt)
    pure $ case (exact, extensionless) of
        (Just tgt, _) -> (path, Just tgt)
        (Nothing, Just (htmlPath, tgt)) -> (htmlPath, Just tgt)
        (Nothing, Nothing) -> (path, Nothing)

{- | The @.html@ URL an extensionless request may stand for: @\/foo\/bar@
stands for @\/foo\/bar.html@. Paths whose last segment is empty (a trailing
slash) or already carries an extension stand for nothing else.
-}
extensionlessHtmlPath :: ByteString -> Maybe ByteString
extensionlessHtmlPath path
    | ByteString.null lastSegment = Nothing
    | ByteString.elem '.' lastSegment = Nothing
    | otherwise = Just (path <> ".html")
  where
    lastSegment = snd (ByteString.breakEnd (== '/') path)

{- | Pages kitchen-sink renders itself (articles from @.cmark@\/@.md@ sources,
and the listings built alongside them), as opposed to files copied verbatim
from the source directory such as raw @.html@ files. Only these are also
served at the URL without @.html@.
-}
isGeneratedPage :: Target ext a -> Bool
isGeneratedPage tgt = case productionRule tgt of
    ProduceAssembler _ -> True
    ProduceGenerator _ -> False
    ProduceFileCopy _ -> False

data OnTheFlyCounters a
    = OnTheFlyCounters
    { incTargetRequests :: (Text, Text) -> IO ()
    , timeBuild :: (Text) -> IO a -> IO a
    }

ontheflyCounters :: Counters -> OnTheFlyCounters a
ontheflyCounters cntrs =
    OnTheFlyCounters
        (\labels -> Prometheus.withLabel cntrs.cnt_targetRequests labels Prometheus.incCounter)
        (\labels work -> timeItWithLabel cntrs.time_ontheflybuild labels work)

handleOnTheFlyProduction ::
    forall ext.
    (Show ext, Typeable ext) =>
    FetchTarget ext ->
    OnTheFlyCounters (LByteString.ByteString, Int64) ->
    Prod.Tracer.Tracer IO (DevServerTrack ext) ->
    Application
handleOnTheFlyProduction fetchTarget cntrs2 track = go
  where
    go :: Application
    go req resp = do
        let origpath = requestedPath req
        (path, target) <- fetchTarget origpath
        maybe
            (handleNotFound origpath resp)
            (handleFound path resp)
            target

    handleNotFound :: RequestedPath -> (Wai.Response -> IO a) -> IO a
    handleNotFound path resp = do
        cntrs2.incTargetRequests ("not-found", Text.decodeUtf8 $ coerce path)
        runTracer track (TargetMissing $ coerce path)
        resp $ Wai.responseLBS status404 [] "not found"

    handleFound :: TargetPath -> (Wai.Response -> IO a) -> Target ext () -> IO a
    handleFound path resp tgt = do
        cntrs2.incTargetRequests ("found", Text.decodeUtf8 path)
        let produce work = cntrs2.timeBuild (destinationUrl $ destination tgt) work
        (body, size) <- produce $ do
            body <- LByteString.fromStrict <$> outputTarget (blogTargetTracer track) tgt
            let size = LByteString.length body
            seq size (pure (body, size))
        runTracer track (TargetBuilt path size)
        resp $ Wai.responseLBS status200 (("content-type", ctypeFor path) : corsHeadersFor path) body

    ctypeFor path
        | path == "/.well-known/webfinger" = "application/jrd+json"
        | ".js" `ByteString.isSuffixOf` path = "application/javascript"
        | ".json" `ByteString.isSuffixOf` path = "application/json"
        | ".html" `ByteString.isSuffixOf` path = "text/html"
        | True = ""

    -- JSON data targets (e.g. `topicsgraph.json`, consumed by the
    -- graphexplorer widget to build a federated cross-site graph, see
    -- KitchenSink.Layout.Blog.Analyses.SiteGraph) are meant to be readable
    -- from other kitchen-sink sites' origins. They carry no secrets and no
    -- credentials/cookies are involved, so a permissive read-only CORS
    -- allowance is safe: allow any origin to GET them.
    corsHeadersFor path
        | ".json" `ByteString.isSuffixOf` path = [("Access-Control-Allow-Origin", "*")]
        | True = []
