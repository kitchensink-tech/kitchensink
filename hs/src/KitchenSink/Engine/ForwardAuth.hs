{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Forward authentication, in the style of Traefik's ForwardAuth and nginx's
@auth_request@: before a request reaches the site, a subrequest goes to an
external auth service (oauth2-proxy, Authelia, Pomerium, a small custom
service, ...), which decides.

* The subrequest is a @GET@ (no body) to the configured URL, carrying the
  original request's headers (cookies, @Authorization@, ...) minus the
  hop-by-hop ones, plus @X-Forwarded-Method@, @X-Forwarded-Proto@,
  @X-Forwarded-Host@ and @X-Forwarded-Uri@. Redirects are not followed.
* A 2xx answer lets the request through. The headers named in
  'responseHeaders' (e.g. @X-Forwarded-User@) are copied from the auth answer
  onto the request; a client-sent header of the same name is always
  dropped, so it cannot be forged.
* Any other answer is relayed to the client as is: status, headers (notably
  @Location@ and @Set-Cookie@) and body. A 302 to an IdP login page or a 401
  with @WWW-Authenticate@ just work.
* An auth service that cannot be reached, or does not answer in time,
  fails closed with a 503: the content is never served.

The check covers the whole site (every path the daemon answers). There is
no per-path rule yet; 'ForwardAuthConfig' is a JSON object so that path
prefixes can be added later without breaking configs.
-}
module KitchenSink.Engine.ForwardAuth (
    ForwardAuthConfig (..),
    ForwardAuth,
    initForwardAuth,
    forwardAuthMiddleware,
) where

import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as LByteString
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (HeaderName, RequestHeaders, ResponseHeaders, status503)
import Network.HTTP.Types.Status (statusIsSuccessful)
import Network.Wai qualified as Wai
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Prelude ((*), (||))

import KitchenSink.Prelude

-- | Where and how to check requests; see the module header.
data ForwardAuthConfig = ForwardAuthConfig
    { url :: Text
    -- ^ the auth service, e.g. @http://127.0.0.1:4180/oauth2/auth@
    , responseHeaders :: Maybe [Text]
    -- ^ headers copied from a 2xx auth answer onto the request (absent: none)
    , timeoutSeconds :: Maybe Int
    -- ^ how long to wait for the auth service (absent: 'defaultTimeoutSeconds')
    }
    deriving (Generic, Show, FromJSON, ToJSON)

defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 10

-- | A 'ForwardAuthConfig' whose URL has been parsed, with its HTTP manager.
data ForwardAuth = ForwardAuth
    { baseRequest :: Http.Request
    , copied :: [HeaderName]
    , manager :: Http.Manager
    }

{- | Checks the configuration once, at startup: an auth URL that does not
parse stops the daemon rather than failing every request later.
-}
initForwardAuth :: Http.Manager -> ForwardAuthConfig -> IO ForwardAuth
initForwardAuth mgr cfg =
    case Http.parseRequest (Text.unpack cfg.url) of
        Nothing -> do
            hPutStrLn stderr ("forward-auth: invalid auth URL: " <> Text.unpack cfg.url)
            exitFailure
        Just req -> do
            let timeout = fromMaybe defaultTimeoutSeconds cfg.timeoutSeconds
            let base =
                    req
                        { Http.method = "GET"
                        , Http.redirectCount = 0
                        , Http.responseTimeout = Http.responseTimeoutMicro (timeout * 1000000)
                        }
            pure $ ForwardAuth base (fmap headerName (fromMaybe [] cfg.responseHeaders)) mgr
  where
    headerName :: Text -> HeaderName
    headerName = fromString . Text.unpack

-- | Runs the check described in the module header before the application.
forwardAuthMiddleware :: ForwardAuth -> Wai.Middleware
forwardAuthMiddleware fa app req respond = do
    result <- try @Http.HttpException (Http.httpLbs (authRequest fa req) fa.manager)
    case result of
        Left err -> do
            hPutStrLn stderr ("forward-auth: auth service unreachable: " <> show err)
            respond $ Wai.responseLBS status503 [("content-type", "text/plain; charset=utf-8")] "authentication service unavailable"
        Right rsp
            | statusIsSuccessful (Http.responseStatus rsp) ->
                app (withCopiedHeaders fa.copied (Http.responseHeaders rsp) req) respond
            | otherwise ->
                respond $
                    Wai.responseLBS
                        (Http.responseStatus rsp)
                        (relayedHeaders (Http.responseHeaders rsp))
                        (Http.responseBody rsp)

authRequest :: ForwardAuth -> Wai.Request -> Http.Request
authRequest fa req =
    fa.baseRequest
        { Http.requestHeaders = forwarded <> List.filter (keep . fst) (Wai.requestHeaders req)
        , Http.requestBody = Http.RequestBodyLBS LByteString.empty
        }
  where
    forwarded :: RequestHeaders
    forwarded =
        [ ("X-Forwarded-Method", Wai.requestMethod req)
        , ("X-Forwarded-Proto", if Wai.isSecure req then "https" else "http")
        , ("X-Forwarded-Uri", Wai.rawPathInfo req <> Wai.rawQueryString req)
        ]
            <> maybe [] (\h -> [("X-Forwarded-Host", h)]) (Wai.requestHeaderHost req)

    -- the auth service gets its own Host, length and connection handling,
    -- and the X-Forwarded-* headers set above, not client-sent ones
    keep :: HeaderName -> Bool
    keep h = not (h `List.elem` (hopByHop <> fmap fst forwarded <> ["X-Forwarded-Host", "host", "content-length", "content-type", "expect"]))

withCopiedHeaders :: [HeaderName] -> ResponseHeaders -> Wai.Request -> Wai.Request
withCopiedHeaders names authHeaders req =
    req
        { Wai.requestHeaders =
            List.filter (\(h, _) -> h `List.notElem` names) (Wai.requestHeaders req)
                <> List.filter (\(h, _) -> h `List.elem` names) authHeaders
        }

-- | The auth answer's headers, minus those that describe the auth
-- connection rather than the answer (the body is re-sent in full, already
-- decoded).
relayedHeaders :: ResponseHeaders -> ResponseHeaders
relayedHeaders =
    List.filter (\(h, _) -> not (h `List.elem` hopByHop || h `List.elem` ["content-length", "content-encoding"]))

hopByHop :: [HeaderName]
hopByHop =
    [ "connection"
    , "keep-alive"
    , "proxy-authenticate"
    , "proxy-authorization"
    , "proxy-connection"
    , "te"
    , "trailer"
    , "transfer-encoding"
    , "upgrade"
    ]
