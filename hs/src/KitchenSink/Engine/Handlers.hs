{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.Handlers where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import Data.Text (Text)
import Network.HTTP.Types (status404)
import Network.Wai as Wai
import Prelude (id,unlines)
import Prod.Tracer
import Prod.Status
import Prod.Prometheus (timeIt)
import Prod.Background as Background
import qualified Prod.Status as Prod
import qualified Prod.Proxy as ProdProxy
import qualified Prometheus as Prometheus
import Servant
import System.Process (readCreateProcess, proc)

import KitchenSink.Prelude
import KitchenSink.Core.Build.Target (destinationUrl, destination)
import KitchenSink.Core.Build.Trace as Build
import KitchenSink.Engine.SiteBuilder
import KitchenSink.Engine.Api
import KitchenSink.Engine.Config
import KitchenSink.Engine.Counters (Counters(..))
import KitchenSink.Engine.SiteLoader (Site)
import KitchenSink.Engine.Track (DevServerTrack(..), WatchResult(..), RequestedPath(..))
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.OnTheFly

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
  let res = if targetStillExists
            then Reloaded
            else Disappeared
  runTracer (traceDev rt) (WatchLeft pathname res)
  pure (Prod.this, res)

handleDevListTargets :: Engine ext -> Runtime ext -> Handler [Text]
handleDevListTargets engine rt = liftIO $ do
  site <- readBackgroundVal (liveSite rt)
  meta <- execLoadMetaExtradata engine
  let tgts = evalTargets engine meta site
  pure $ [ destinationUrl $ destination tgt | tgt <- tgts ]

handleDevProduce :: (Show ext, Typeable ext) => Engine ext -> Runtime ext -> Handler DevTextOutput
handleDevProduce engine rt = liftIO $ do
  site <- readBackgroundVal (liveSite rt)
  extra <- execLoadMetaExtradata engine
  let tgts = evalTargets engine extra site
  log <- newIORef []
  let traceTargets item = do
          let blogTracer = contramap BlogTargetTrace $ (traceDev rt)
          atomicModifyIORef' log (\items -> (item : items,()))
          runTracer blogTracer item
  let go tgt = do
          Prometheus.withLabel (time_singlebuild $ counters rt) (destinationUrl $ destination tgt) $ \m -> timeIt id m $ produceTarget traceTargets tgt
  timeIt time_fullbuild (counters rt) $
    mapConcurrently_ go tgts
  Prometheus.incCounter $ cnt_rebuilds $ counters rt
  runTracer (traceDev rt) ProducedBuild
  let collateLogs = Text.pack . unlines . fmap show :: [Build.Trace] -> Text
  DevTextOutput . collateLogs <$> readIORef log

handleDevPublish :: Config -> Runtime ext -> Handler DevTextOutput
handleDevPublish config rt =
    case publishScript config of
      Just path -> runPublishScript path
      Nothing -> pure $ DevTextOutput "no publish-script configured"
  where
    runPublishScript path = liftIO $ do
      out <- timeIt time_publishing (counters rt) $ do
        procOut <- readCreateProcess (proc path []) ""
        seq (length procOut) $ pure procOut
      Prometheus.incCounter $ cnt_publishs $ counters rt
      runTracer (traceDev rt) (PublishedBuild out)
      pure (DevTextOutput $ Text.pack out)

handleExecCommand :: Config -> Runtime ext -> Text -> Handler DevTextOutput
handleExecCommand config rt commandName =
    case List.find (\c -> handle c == commandName) $ commands config of
      Just cmd -> runCommand cmd
      Nothing -> pure $ DevTextOutput "no publish-script configured"
  where
    runCommand cmd = liftIO $ do
      let path =  exe cmd
      out <- timeIt time_publishing (counters rt) $ do
        procOut <- readCreateProcess (proc path []) ""
        seq (length procOut) $ pure procOut
      Prometheus.withLabel (cnt_commands $ counters rt) (handle cmd) Prometheus.incCounter
      runTracer (traceDev rt) (CommandRan cmd out)
      pure (DevTextOutput $ Text.pack out)

handleDevListCommands :: Config -> Runtime ext -> Handler [Command]
handleDevListCommands config _ =
  pure $ commands config

handleDevForceReload :: Runtime ext -> Handler (Maybe ForceReloadStatus)
handleDevForceReload rt = do
    (_, worked) <- liftIO $ do
      Prometheus.incCounter $ cnt_forceReloads $ counters rt
      reloadSite rt
    let status = if worked then Just ForceReloaded else Nothing
    pure status

handleProxyApi :: Runtime ext -> Application
handleProxyApi rt = case prodproxyRuntime rt of
  Nothing ->
      \_ resp -> resp $ Wai.responseLBS status404 [] "api proxy runtime is disabled"
  Just proxyrt -> do
    ProdProxy.handleProxy proxyrt

-- works with two configured engines: one for all of dev stuff and one in serve mode (for producing files)
serveDevApi :: forall ext. (Show ext, Typeable ext) => Config -> Engine ext -> Engine ext -> Runtime ext -> Server DevApi
serveDevApi config devengine prodengine rt =
  handleDevWatch devengine rt
  :<|> handleDevListTargets devengine rt
  :<|> handleDevProduce prodengine rt
  :<|> handleDevPublish config rt
  :<|> handleDevListCommands config rt
  :<|> handleExecCommand config rt
  :<|> handleDevForceReload rt
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
