{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.Handlers where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Real (fromIntegral)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.ReverseProxy
import Network.Wai as Wai
import Prelude (id,unlines)
import Prod.Tracer
import Prod.Status
import Prod.Prometheus (timeIt)
import Prod.Background as Background
import qualified Prod.Status as Prod
import qualified Prometheus as Prometheus
import Servant
import System.Process (readCreateProcess, proc)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Target hiding (Tracer)
import KitchenSink.Engine.Api
import KitchenSink.Engine.Config
import KitchenSink.Engine.Counters (Counters(..), timeItWithLabel)
import KitchenSink.Engine.Track (DevServerTrack(..), WatchResult(..))
import KitchenSink.Engine.Runtime

handleDevWatch :: Engine -> Runtime -> Maybe Text -> Maybe TargetPathName -> Handler (Prod.Identification, WatchResult)
handleDevWatch _ rt srvId pathname
  | srvId /= Just (coerce Prod.this) = liftIO $ do
    Prometheus.withLabel (cnt_watches $ counters rt) "outdated" Prometheus.incCounter
    runTracer (traceDev rt) (WatchRejected srvId pathname)
    pure (Prod.this, Respawned)
handleDevWatch engine rt _ pathname = liftIO $ do
  Prometheus.withLabel (cnt_watches $ counters rt) "matching" Prometheus.incCounter
  runTracer (traceDev rt) (WatchAdded (coerce Prod.this) pathname)
  waitReload rt
  targetStillExists <- isJust <$> maybe (pure Nothing) (fmap snd . findTarget engine rt . Text.encodeUtf8) pathname
  let res = if targetStillExists
            then Reloaded
            else Disappeared
  runTracer (traceDev rt) (WatchLeft pathname res)
  pure (Prod.this, res)

handleDevListTargets :: Engine -> Runtime -> Handler [Text]
handleDevListTargets engine rt = liftIO $ do
  site <- readBackgroundVal (liveSite rt)
  meta <- execLoadMetaExtradata engine
  let tgts = evalTargets engine meta site
  pure $ [ destinationUrl $ destination tgt | tgt <- tgts ]

handleDevProduce :: Engine -> Runtime -> Handler DevTextOutput
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
  let collateLogs = Text.pack . unlines . fmap show :: [KitchenSink.Blog.Target.Trace] -> Text
  DevTextOutput . collateLogs <$> readIORef log

handleDevPublish :: Config -> Runtime -> Handler DevTextOutput
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

handleExecCommand :: Config -> Runtime -> Text -> Handler DevTextOutput
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

handleDevListCommands :: Config -> Runtime -> Handler [Command]
handleDevListCommands config _ =
  pure $ commands config

handleDevForceReload :: Runtime -> Handler (Maybe ForceReloadStatus)
handleDevForceReload rt = do
    (_, worked) <- liftIO $ do
      Prometheus.incCounter $ cnt_forceReloads $ counters rt
      tryPushNewSite rt
    let status = if worked then Just ForceReloaded else Nothing
    pure status

handleOnTheFlyProduction :: Counters -> Tracer IO DevServerTrack -> (ByteString -> IO (ByteString, Maybe (Target ()))) -> Application
handleOnTheFlyProduction cntrs track fetchTarget = go
  where
    go :: Application
    go req resp = do
      let origpath = Wai.rawPathInfo req
      (path, target) <- fetchTarget origpath
      maybe
        (handleNotFound origpath resp)
        (handleFound path resp)
        target

    handleNotFound path resp = do
      Prometheus.withLabel (cnt_targetRequests cntrs) ("not-found", Text.decodeUtf8 path) Prometheus.incCounter
      runTracer track (TargetMissing path)
      resp $ Wai.responseLBS status404 [] "not found"

    handleFound path resp tgt = do
      Prometheus.withLabel (cnt_targetRequests cntrs) ("found", Text.decodeUtf8 path) Prometheus.incCounter
      let produce :: IO x -> IO x
          produce work = timeItWithLabel (time_ontheflybuild cntrs) (destinationUrl $ destination tgt) work
      (body,size) <- produce $ do
        body <- LByteString.fromStrict <$> outputTarget tgt
        let size = LByteString.length body
        seq size (pure (body,size))
      runTracer track (TargetBuilt path size)
      Prometheus.withLabel (cnt_targetSizes cntrs) (Text.decodeUtf8 path) (flip Prometheus.setGauge $ fromIntegral size)
      resp $ Wai.responseLBS status200 [("content-type", ctypeFor path)] body

    ctypeFor path
      | ".js" `ByteString.isSuffixOf` path = "application/javascript"
      | ".json" `ByteString.isSuffixOf` path = "application/json"
      | ".html" `ByteString.isSuffixOf` path = "text/html"
      | True = ""

findTarget :: Engine -> Runtime -> ByteString -> IO (ByteString, Maybe (Target ()))
findTarget engine rt origpath = do
  let path = if origpath == "/" then "/index.html" else origpath
  runTracer (traceDev rt) (TargetRequested origpath)
  site <- readBackgroundVal (liveSite rt)
  meta <- execLoadMetaExtradata engine
  let tgts = evalTargets engine meta site
  let target = List.find (\tgt -> Text.encodeUtf8 (destinationUrl (destination tgt)) == path) tgts
  pure (path, target)

serveDevApi :: Config -> Engine -> Engine -> Runtime -> Server DevApi
serveDevApi config devengine prodengine rt =
  handleDevWatch devengine rt
  :<|> handleDevListTargets devengine rt
  :<|> handleDevProduce prodengine rt
  :<|> handleDevPublish config rt
  :<|> handleDevListCommands config rt
  :<|> handleExecCommand config rt
  :<|> handleDevForceReload rt
  :<|> coerce (handleProxyApi config rt)
  :<|> coerce (handleOnTheFlyProduction (counters rt) (traceDev rt) (findTarget devengine rt))

serveApi :: Config -> Engine -> Runtime -> Server ServeApi
serveApi config engine rt =
  coerce (handleProxyApi config rt)
  :<|> coerce (handleOnTheFlyProduction (counters rt) (traceDev rt) (findTarget engine rt))

handleProxyApi :: Config -> Runtime -> Application
handleProxyApi cfg rt = case api cfg of
  Nothing ->
      \_ resp -> resp $ Wai.responseLBS status404 [] "not found"
  Just (host, port) ->
    waiProxyTo (const $ pure $ WPRProxyDest $ ProxyDest (Text.encodeUtf8 host) port) defaultOnExc (httpManager rt)
