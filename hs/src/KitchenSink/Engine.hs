{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Options.Generic
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import GHC.Float (int2Double)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import System.Process (readCreateProcess, proc)

import KitchenSink.Blog
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.SiteLoader as SiteLoader
import KitchenSink.Blog.Target hiding (Tracer)
import KitchenSink.Blog.Layout
import Prelude (id,seq,Read,unlines,error)

import Servant

import Prod.Tracer
import Prod.App as Prod
import Prod.Status
import Prod.Prometheus (timeIt)
import Prod.Background as Background
import qualified Prod.Status as Prod

import Data.Maybe (fromMaybe, isJust)
import Data.ByteString (ByteString)
import Network.HTTP.Client as HTTP (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.ReverseProxy
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Prometheus as Prometheus
import GHC.Real (fromIntegral, fromRational, toRational)

import qualified Paths_prodapi
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString

import Lucid as Lucid
import qualified Lucid.Base as Lucid
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)

import qualified System.FSNotify as FSNotify
import System.FilePath.Posix (takeExtension, (</>))
import Control.Monad (when)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import GHC.Enum (succ)

data ServMode = SERVE | DEV
  deriving (Generic, Read, Show)

instance ParseField ServMode
instance ParseFields ServMode
instance ParseRecord ServMode

data Action
  = Produce
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    }
  | Serve
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    , servMode :: ServMode <?> "SERVE|DEV" 
    , port   :: Int <?> "port-num"
    }
  deriving (Generic, Show)

instance ParseRecord Action

data DevServerTrack
  = ProducedBuild
  | PublishedBuild String
  | WatchAdded Text (Maybe Text)
  | WatchRejected (Maybe Text) (Maybe Text)
  | WatchLeft (Maybe Text) WatchResult
  | FileWatch FSNotify.Event
  | SiteReloaded (Background.Track ())
  | TargetRequested ByteString
  | TargetMissing ByteString
  | TargetBuilt ByteString Int64
  | Loading SiteLoader.LogMsg
  | BlogTargetTrace KitchenSink.Blog.Target.Trace
  deriving Show

data DevServerRuntime = DevServerRuntime
  { waitReload :: IO ()
  , tryPushNewSite :: IO (Site, Bool)
  , traceDev :: Tracer IO DevServerTrack
  , liveSite :: BackgroundVal Site
  , counters :: Counters
  , httpManager :: HTTP.Manager
  }

initDevServerRuntime :: Engine -> FilePath -> Tracer IO DevServerTrack -> IO DevServerRuntime
initDevServerRuntime engine path devtracer = do
  rtCounters <- initCounters
  siteTMVar <- newEmptyTMVarIO
  watches <- newTVarIO []

  notify <- FSNotify.startManager
  _ <- FSNotify.watchDir notify path notifyPredicate (handleFSEvent rtCounters siteTMVar)
  
  DevServerRuntime
    <$> pure (waitFsChanges watches)
    <*> pure (pushNewSite rtCounters siteTMVar)
    <*> pure devtracer
    <*> (load rtCounters >>= (\s -> background (contramap f devtracer) 0 s (reloadSite rtCounters siteTMVar watches)))
    <*> pure rtCounters
    <*> newManager defaultManagerSettings
  where
    load :: Counters -> IO Site
    load cntrs = do
      site <- execLoadSite engine
      Prometheus.setGauge (cnt_sources cntrs) (int2Double $ countSources $ site)
      pure site

    pushNewSite :: Counters -> TMVar Site -> IO (Site, Bool)
    pushNewSite cntrs siteTMVar = do
      site <- load cntrs
      res <- seq site $ atomically $ tryPutTMVar siteTMVar site
      pure (site, res)
 
    reloadSite :: Counters -> TMVar Site -> TVar [TMVar ()] -> Int -> IO (Site, Int)
    reloadSite cntrs siteTMVar watches v = do
      Prometheus.incCounter $ cnt_reloads cntrs
      -- wait for siteTMVar and fan-out to all watches
      atomically $ do
        site <- takeTMVar siteTMVar
        old <- swapTVar watches []
        traverse_ (flip putTMVar ()) old
        pure (site, succ v)

    waitFsChanges :: TVar [TMVar ()] -> IO ()
    waitFsChanges watches = do
      w <- newEmptyTMVarIO
      -- append itself to watches
      atomically $ do
        modifyTVar' watches (\ws -> w:ws)
      void . atomically $ takeTMVar w

    notifyPredicate :: FSNotify.Event -> Bool
    notifyPredicate ev = case ev of
      FSNotify.Added _ _ _ -> True
      FSNotify.CloseWrite _ _ _ -> True
      FSNotify.Modified _ _ _ -> True
      FSNotify.ModifiedAttributes _ _ _ -> False
      FSNotify.Unknown _ _ _ _ -> True
      FSNotify.Removed _ _ _ -> False
      FSNotify.WatchedDirectoryRemoved _ _ _ -> False

    getFilePath :: FSNotify.Event -> FilePath
    getFilePath ev = case ev of
      FSNotify.Added p _ _ -> p
      FSNotify.CloseWrite p _ _ -> p
      FSNotify.Modified p _ _ -> p
      FSNotify.ModifiedAttributes p _ _ -> p
      FSNotify.Removed p _ _ -> p
      FSNotify.Unknown p _ _ _ -> p
      FSNotify.WatchedDirectoryRemoved p _ _ -> p

    handleFSEvent :: Counters -> TMVar Site -> FSNotify.Event -> IO ()
    handleFSEvent cntrs x ev = do
      let modfile = getFilePath ev
      let shouldReload = (not . ignoreFileReload $ modfile)
      when shouldReload $ do
        runTracer devtracer $ FileWatch ev
        site <- threadDelay 100000 >> load cntrs --give some time to FS to sync
        atomically $ do
          _ <- tryTakeTMVar x
          putTMVar x site

    ignoreFileReload :: FilePath -> Bool
    ignoreFileReload p = takeExtension p == ".swp"

    f :: Track Site -> DevServerTrack
    f trk = SiteReloaded $ fmap (const ()) trk

data WatchResult
  = Reloaded
  | Disappeared
  | Respawned
  deriving (Generic, Show)
instance ToJSON WatchResult

type DevApi = DevWatchApi
  :<|> DevListTargetsApi
  :<|> DevProduceApi
  :<|> DevPublishApi
  :<|> DevListCommandsApi
  :<|> DevExecCommandApi
  :<|> DevForceReloadApi
  :<|> ProxyApi
  :<|> OnTheFlyProductionApi

type ServeApi = ProxyApi
  :<|> OnTheFlyProductionApi

type TargetPathName = Text

type DevWatchApi = "dev" :> "watch" :> QueryParam "server-id" Text :> QueryParam "pathname" TargetPathName :> Get '[JSON] (Prod.Identification, WatchResult)

type DevListTargetsApi = "dev" :> "targets" :> Get '[JSON] [Text]

newtype DevTextOutput = DevTextOutput Text
  deriving (Generic, Show)
instance ToJSON DevTextOutput

type DevProduceApi = "dev" :> "produce" :> Post '[JSON] DevTextOutput

type DevPublishApi = "dev" :> "publish" :> Post '[JSON] DevTextOutput

type DevListCommandsApi = "dev" :> "commands" :> Get '[JSON] [Command]

type DevExecCommandApi = "dev" :> "command" :> QueryParam' '[Required,Strict] "handle" Text :> Post '[JSON] DevTextOutput

data ForceReloadStatus = ForceReloaded
  deriving (Generic, Show)
instance ToJSON ForceReloadStatus

type DevForceReloadApi = "dev" :> "reload" :> Post '[JSON] (Maybe ForceReloadStatus)

type ProxyApi = "api" :> Raw

type OnTheFlyProductionApi = Raw

handleDevWatch :: Engine -> DevServerRuntime -> Maybe Text -> Maybe TargetPathName -> Handler (Prod.Identification, WatchResult)
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

handleDevListTargets :: Engine -> DevServerRuntime -> Handler [Text]
handleDevListTargets engine rt = liftIO $ do
  site <- readBackgroundVal (liveSite rt)
  meta <- execLoadMetaExtradata engine
  let tgts = evalTargets engine meta site
  pure $ [ destinationUrl $ destination tgt | tgt <- tgts ]

handleDevProduce :: Engine -> DevServerRuntime -> Handler DevTextOutput
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

handleDevPublish :: KitchenSinkEngineConfig -> DevServerRuntime -> Handler DevTextOutput
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

handleExecCommand :: KitchenSinkEngineConfig -> DevServerRuntime -> Text -> Handler DevTextOutput
handleExecCommand config rt commandName =
    case List.find (\c -> handle c == commandName) $ commands config of
      Just cmd -> runCommand $ exe cmd
      Nothing -> pure $ DevTextOutput "no publish-script configured"
  where
    runCommand path = liftIO $ do
      out <- timeIt time_publishing (counters rt) $ do
        procOut <- readCreateProcess (proc path []) ""
        seq (length procOut) $ pure procOut
      Prometheus.incCounter $ cnt_publishs $ counters rt
      runTracer (traceDev rt) (PublishedBuild out)
      pure (DevTextOutput $ Text.pack out)

handleDevListCommands :: KitchenSinkEngineConfig -> DevServerRuntime -> Handler [Command]
handleDevListCommands config _ =
  pure $ commands config

handleDevForceReload :: DevServerRuntime -> Handler (Maybe ForceReloadStatus)
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

findTarget :: Engine -> DevServerRuntime -> ByteString -> IO (ByteString, Maybe (Target ()))
findTarget engine rt origpath = do
  let path = if origpath == "/" then "/index.html" else origpath
  runTracer (traceDev rt) (TargetRequested origpath)
  site <- readBackgroundVal (liveSite rt)
  meta <- execLoadMetaExtradata engine
  let tgts = evalTargets engine meta site
  let target = List.find (\tgt -> Text.encodeUtf8 (destinationUrl (destination tgt)) == path) tgts
  pure (path, target)

loadJSONFile :: FromJSON a => FilePath -> IO (Maybe a)
loadJSONFile path =
  decode <$> LByteString.readFile path

data Command = Command {
    exe  :: FilePath
  , display :: Text
  , handle :: Text
  } deriving (Generic, Show)
instance FromJSON Command
instance ToJSON Command

type HostName = Text
type PortNum = Int
data KitchenSinkEngineConfig = KitchenSinkEngineConfig {
    publishScript :: Maybe FilePath
  , commands :: [Command]
  , api :: Maybe (HostName, PortNum)
  } deriving (Generic, Show)
instance FromJSON KitchenSinkEngineConfig

data LinkedSiteConfig = LinkedSiteConfig {
    baseURL :: Text
  , siteType :: Text
  , siteTitle :: Text
  } deriving (Generic, Show)
instance FromJSON LinkedSiteConfig

data GlobalSiteConfig = GlobalSiteConfig {
    title      :: Text
  , publishURL :: Text
  , twitterLogin :: Maybe Text
  , linkedSites :: Maybe [LinkedSiteConfig]
  } deriving (Generic, Show)
instance FromJSON GlobalSiteConfig

defaultGlobalSiteConfig :: GlobalSiteConfig
defaultGlobalSiteConfig =
  GlobalSiteConfig "invalid siteconfig!" "/" Nothing Nothing

defaultGetExtraData :: FilePath -> IO MetaExtraData
defaultGetExtraData path = do
  config <- fromMaybe defaultGlobalSiteConfig <$> loadJSONFile path
  MetaExtraData
    <$> getCurrentTime
    <*> pure (title config)
    <*> pure (publishURL config)
    <*> pure (twitterLogin config)
    <*> pure noExtraHeaders
    <*> pure (maybe [] (fmap baseURL) $ linkedSites config)
  where
    noExtraHeaders _ = pure mempty

getDevExtraData :: FilePath -> IO MetaExtraData
getDevExtraData path = do
  config <- fromMaybe defaultGlobalSiteConfig <$> loadJSONFile path
  MetaExtraData
    <$> getCurrentTime
    <*> pure (title config)
    <*> pure (publishURL config)
    <*> pure (twitterLogin config)
    <*> pure jsReloadExtraHeaders
    <*> pure (maybe [] (fmap baseURL) $ linkedSites config)
  where
    jsReloadExtraHeaders :: Article [Text] -> Assembler (Lucid.Html ())
    jsReloadExtraHeaders _ =
      let js1 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/autoreload.js" ] ""
          js2 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/add-dev-route.js" ] ""
          js3 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/echarts.min.js" ] ""
          js5 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/echart-histogram.js" ] ""
      in pure (js1 *> js2 *> js3 *> js5)

serveDevApi :: KitchenSinkEngineConfig -> Engine -> Engine -> DevServerRuntime -> Server DevApi
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

serveApi :: KitchenSinkEngineConfig -> Engine -> DevServerRuntime -> Server ServeApi
serveApi config engine rt =
  coerce (handleProxyApi config rt)
  :<|> coerce (handleOnTheFlyProduction (counters rt) (traceDev rt) (findTarget engine rt))

handleProxyApi :: KitchenSinkEngineConfig -> DevServerRuntime -> Application
handleProxyApi cfg rt = case api cfg of
  Nothing ->
      \_ resp -> resp $ Wai.responseLBS status404 [] "not found"
  Just (host, port) ->
    waiProxyTo (const $ pure $ WPRProxyDest $ ProxyDest (Text.encodeUtf8 host) port) defaultOnExc (httpManager rt)

-- primitives required to run an engine
data Engine = Engine {
    execLoadSite :: IO Site
  , execLoadMetaExtradata :: IO MetaExtraData
  , evalTargets :: MetaExtraData -> Site -> [Target ()]
  , execProduceTarget :: Target () -> IO ()
  }

defaultMain :: IO ()
defaultMain = do
  cmd <- getRecord "kitchen-sink"
  let srcPath = coerce $ srcDir cmd
  let kitchensinkFilePath = srcPath </> "kitchen-sink.json"
  let portnum = coerce $ port cmd
  let prodengine = Engine
                  (loadSite (runTracer $ contramap Loading $ tracePrint) srcPath)
                  (defaultGetExtraData kitchensinkFilePath)
                  (\med site -> fmap (fmap $ const ()) $ siteTargets (coerce $ outDir cmd) print med site)
                  (produceTarget print)
  let devengine = prodengine { execLoadMetaExtradata = getDevExtraData kitchensinkFilePath }
  case cmd of
    Produce _ _ -> do
      site <- execLoadSite prodengine
      meta <- execLoadMetaExtradata prodengine
      let tgts = evalTargets prodengine meta site
      traverse_ (execProduceTarget prodengine) tgts
    Serve _ _ mode _ -> do
      case (coerce mode) of
        DEV   -> runDev kitchensinkFilePath devengine prodengine srcPath portnum
        SERVE -> runServe kitchensinkFilePath prodengine srcPath portnum
  where
    runDev kspath devengine prodengine path portnum = do
      ksconfig <- loadJSONFile kspath >>= maybe (error "couldn't load kitchensink.json") pure
      let apiStatus = pure ("ok" :: Text)
      healthRt <- Prod.alwaysReadyRuntime tracePrint
      rt <- initDevServerRuntime devengine path tracePrint
      init <- initialize healthRt
      Warp.run portnum
        $ RequestLogger.logStdoutDev
        $ app
           init
           apiStatus
           (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> metricsSection "js/metrics.js")
           (serveDevApi ksconfig devengine prodengine rt)
           (Proxy @DevApi)

    runServe kspath engine path portnum = do
      ksconfig <- loadJSONFile kspath >>= maybe (error "couldn't load kitchensink.json") pure
      let apiStatus = pure ("ok" :: Text)
      healthRt <- Prod.alwaysReadyRuntime tracePrint
      rt <- initDevServerRuntime engine path tracePrint
      init <- initialize healthRt
      Warp.run portnum
        $ RequestLogger.logStdoutDev
        $ app
           init
           apiStatus
           (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> metricsSection "js/metrics.js")
           (serveApi ksconfig engine rt)
           (Proxy @ServeApi)


data Counters
  = Counters
      { time_singlebuild :: Prometheus.Vector Text Prometheus.Summary
      , time_fullbuild :: Prometheus.Summary
      , time_publishing :: Prometheus.Summary
      , time_ontheflybuild :: Prometheus.Vector Text Prometheus.Summary
      , cnt_reloads :: Prometheus.Counter
      , cnt_watches :: Prometheus.Vector Text Prometheus.Counter
      , cnt_rebuilds :: Prometheus.Counter
      , cnt_publishs :: Prometheus.Counter
      , cnt_targetRequests :: Prometheus.Vector (Text, Text) Prometheus.Counter
      , cnt_targetSizes :: Prometheus.Vector (Text) Prometheus.Gauge
      , cnt_sources :: Prometheus.Gauge
      , cnt_forceReloads :: Prometheus.Counter
      }

initCounters :: IO Counters
initCounters =
  Counters
    <$> reg1s "blog_single_build_time" "path" "time spent building single targets on disk"
    <*> reg0s "blog_fullbuild_time" "time spent building targets on disk"
    <*> reg0s "blog_publish_time" "time spent publishing"
    <*> reg1s "blog_ontheflybuild_time" "path" "time spent building targets on disk"
    <*> reg0 "blog_reloads" "number of time the site is reloaded"
    <*> reg1 "blog_watches" "status" "number of watches created"
    <*> reg0 "blog_rebuilds" "number of time the output has been rebuilt"
    <*> reg0 "blog_publishes" "number of time the site has been published"
    <*> reg1 "blog_targets_requests" ("status","path") "number of queries per blog target"
    <*> reg1g "blog_targets_sizes" ("path") "sizes of targets in bytes"
    <*> reg0g "blog_targets_number" "number of targets"
    <*> reg0 "blog_forceReloads" "number of time the site has been reloaded upon user request"
  where
    reg0 k h =
      Prometheus.register
        $ Prometheus.counter (Prometheus.Info k h)
    reg0g k h =
      Prometheus.register
        $ Prometheus.gauge (Prometheus.Info k h)
    reg1 k t h =
      Prometheus.register
        $ Prometheus.vector t
        $ Prometheus.counter (Prometheus.Info k h)
    reg1g k t h =
      Prometheus.register
        $ Prometheus.vector t
        $ Prometheus.gauge (Prometheus.Info k h)
    reg0s k h =
      Prometheus.register
        $ Prometheus.summary (Prometheus.Info k h) Prometheus.defaultQuantiles
    reg1s k t h =
      Prometheus.register
        $ Prometheus.vector t
        $ Prometheus.summary (Prometheus.Info k h) Prometheus.defaultQuantiles


timeItWithLabel
  :: (Prometheus.Label label, Prometheus.Observer metric)
  => Prometheus.Vector label metric
  -> label
  -> IO b
  -> IO b
timeItWithLabel cnt lbl action = do
  t0 <- liftIO $ getCurrentTime
  !ret <- action
  t1 <- liftIO $ getCurrentTime
  Prometheus.withLabel cnt lbl $ flip Prometheus.observe (fromRational $ toRational $ diffUTCTime t1 t0)
  pure ret

