{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text.Encoding qualified as Text
import Data.Typeable (Typeable)
import Network.HTTP.Types (status404)
import Network.Wai as Wai
import Prod.Background as Background
import Prod.Proxy qualified as ProdProxy
import Prod.Status
import Prod.Status qualified as Prod
import Prod.Tracer
import Prometheus qualified as Prometheus
import Servant

import KitchenSink.Core.Build.Target (destination, destinationUrl)
import KitchenSink.Engine.Api
import KitchenSink.Engine.Counters (Counters (..))
import KitchenSink.Engine.OnTheFly
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.SiteLoader (Site)
import KitchenSink.Engine.Track (DevServerTrack (..), RequestedPath (..), WatchResult (..))
import KitchenSink.Prelude

handleDevWatch :: Engine ext -> Runtime ext -> Maybe Text -> Maybe TargetPathName -> Handler (Prod.Identification, WatchResult)
handleDevWatch _ rt srvId pathname
    | srvId /= Just (coerce Prod.this) = liftIO $ do
        Prometheus.withLabel (cnt_watches $ counters rt) "outdated" Prometheus.incCounter
        runTracer (traceDev rt) (WatchRejected srvId pathname)
        pure (Prod.this, Respawned)
handleDevWatch engine rt _ pathname = liftIO $ do
    Prometheus.withLabel (cnt_watches $ counters rt) "matching" Prometheus.incCounter
    runTracer (traceDev rt) (WatchAdded (coerce Prod.this) pathname)
    waitReload rt
    let readSite = readBackgroundVal (liveSite rt)
    let lookupTarget = fmap snd . findTarget engine readSite (traceDev rt) . RequestedPath . Text.encodeUtf8
    targetStillExists <- isJust <$> maybe (pure Nothing) lookupTarget pathname
    let res =
            if targetStillExists
                then Reloaded
                else Disappeared
    runTracer (traceDev rt) (WatchLeft pathname res)
    pure (Prod.this, res)

handleDevListTargets :: Engine ext -> Runtime ext -> Handler [Text]
handleDevListTargets engine rt = liftIO $ do
    site <- readBackgroundVal (liveSite rt)
    meta <- execLoadMetaExtradata engine
    let tgts = evalTargets engine meta site
    pure $ [destinationUrl $ destination tgt | tgt <- tgts]

handleProxyApi :: Runtime ext -> Application
handleProxyApi rt = case prodproxyRuntime rt of
    Nothing ->
        \_ resp -> resp $ Wai.responseLBS status404 [] "api proxy runtime is disabled"
    Just proxyrt -> do
        ProdProxy.handleProxy proxyrt

-- works with two configured engines: one for all of dev stuff and one in serve mode (for producing files)
serveDevApi :: forall ext. (Show ext, Typeable ext) => Engine ext -> Runtime ext -> Server DevApi
serveDevApi devengine rt =
    handleDevWatch devengine rt
        :<|> handleDevListTargets devengine rt
        :<|> coerce (handleProxyApi rt)
        :<|> coerce (handleOnTheFlyProduction (findTarget devengine readSite (traceDev rt)) (ontheflyCounters rt.counters) (traceDev rt))
  where
    readSite :: IO (Site ext)
    readSite = readBackgroundVal (liveSite rt)

serveApi :: forall ext. (Show ext, Typeable ext) => Engine ext -> Runtime ext -> Server ServeApi
serveApi engine rt =
    coerce (handleProxyApi rt)
        :<|> coerce (handleOnTheFlyProduction (findTarget engine readSite (traceDev rt)) (ontheflyCounters rt.counters) (traceDev rt))
  where
    readSite :: IO (Site ext)
    readSite = readBackgroundVal (liveSite rt)
