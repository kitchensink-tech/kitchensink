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
import Data.Typeable (Typeable)
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

import KitchenSink.Prelude
import KitchenSink.Core.Build.Target (Target, destinationUrl, destination)
import KitchenSink.Core.Build.Trace as Build
import KitchenSink.Engine.SiteBuilder
import KitchenSink.Engine.Api
import KitchenSink.Engine.Config
import KitchenSink.Engine.Counters (Counters(..), timeItWithLabel)
import KitchenSink.Engine.Track (DevServerTrack(..), WatchResult(..), RequestedPath(..), rootRequestPath, requestedPath, blogTargetTracer)
import KitchenSink.Engine.Runtime

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
  let lookupTarget = fmap snd . findTarget engine rt . RequestedPath . Text.encodeUtf8
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

handleOnTheFlyProduction :: forall ext. (Show ext, Typeable ext) => Runtime ext -> FetchTarget ext -> Application
handleOnTheFlyProduction rt fetchTarget = go
  where
    cntrs = counters rt
    track = traceDev rt

    go :: Application
    go req resp = do
      let origpath = requestedPath req
      (path, target) <- fetchTarget origpath
      maybe
        (handleNotFound origpath resp)
        (handleFound path resp)
        target

    handleNotFound :: RequestedPath -> (Response -> IO a) -> IO a
    handleNotFound path resp = do
      Prometheus.withLabel (cnt_targetRequests cntrs) ("not-found", Text.decodeUtf8 $ coerce path) Prometheus.incCounter
      runTracer track (TargetMissing $ coerce path)
      resp $ Wai.responseLBS status404 [] "not found"

    handleFound :: TargetPath -> (Response -> IO a) -> Target ext () -> IO a
    handleFound path resp tgt = do
      Prometheus.withLabel (cnt_targetRequests cntrs) ("found", Text.decodeUtf8 path) Prometheus.incCounter
      let produce :: IO x -> IO x
          produce work = timeItWithLabel (time_ontheflybuild cntrs) (destinationUrl $ destination tgt) work
      (body,size) <- produce $ do
        body <- LByteString.fromStrict <$> outputTarget (blogTargetTracer track) tgt
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

handleProxyApi :: Config -> Runtime ext -> Application
handleProxyApi cfg rt = case api cfg of
  Nothing ->
      \_ resp -> resp $ Wai.responseLBS status404 [] "not found"
  Just (host, port) ->
    waiProxyTo (const $ pure $ WPRProxyDest $ ProxyDest (Text.encodeUtf8 host) port) defaultOnExc (httpManager rt)

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
  :<|> coerce (handleProxyApi config rt)
  :<|> coerce (handleOnTheFlyProduction rt (findTarget devengine rt))

serveApi :: forall ext. (Show ext, Typeable ext) => Config -> Engine ext -> Runtime ext -> Server ServeApi
serveApi config engine rt =
  coerce (handleProxyApi config rt)
  :<|> coerce (handleOnTheFlyProduction rt (findTarget engine rt))

type TargetPath = ByteString

type FetchTarget ext = RequestedPath -> IO (TargetPath, Maybe (Target ext ()))

findTarget :: Engine ext -> Runtime ext -> FetchTarget ext
findTarget engine rt = \origpath -> do
  runTracer (traceDev rt) (TargetRequested origpath)
  let path = if origpath == rootRequestPath then "/index.html" else coerce origpath
  site <- readBackgroundVal (liveSite rt)
  meta <- execLoadMetaExtradata engine
  let tgts = evalTargets engine meta site
  let target = List.find (\tgt -> Text.encodeUtf8 (destinationUrl (destination tgt)) == path) tgts
  pure (path, target)
