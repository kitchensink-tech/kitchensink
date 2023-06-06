
module KitchenSink.Engine.Runtime
  ( Engine(..)
  , Runtime(..)
  , initDevServerRuntime
  ) where

import Control.Concurrent (forkIO,threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (forever,when)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import qualified Data.Text.Encoding as Text
import GHC.Enum (succ)
import GHC.Float (int2Double)
import Network.HTTP.Client as HTTP (Manager, newManager, defaultManagerSettings)
import Prod.Tracer
import Prod.Background as Background
import qualified Prod.Proxy as ProdProxy
import qualified Prometheus as Prometheus
import qualified System.FSNotify as FSNotify
import System.FilePath.Posix (takeExtension)

import KitchenSink.Prelude
import KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Core.Build.Target (Target)
import KitchenSink.Layout.Blog
import KitchenSink.Engine.Counters (Counters(..), initCounters)
import KitchenSink.Engine.Config
import KitchenSink.Engine.Track (DevServerTrack(..))

-- | Part of the Engine that is shared between one-off production script and
-- longstanding dev-servers.
data Engine ext = Engine {
    execLoadSite :: IO (Site ext)
  , execLoadMetaExtradata :: IO MetaData
  , evalTargets :: MetaData -> (Site ext) -> [Target ext ()]
  , execProduceTarget :: Target ext () -> IO ()
  }

-- | Runtime concerned with auto-reloading, logging and other concernts.
data Runtime ext = Runtime
  { waitReload :: IO ()
  , reloadSite :: IO (Site ext, Bool)
  , traceDev :: Tracer IO (DevServerTrack ext)
  , liveSite :: BackgroundVal (Site ext)
  , counters :: Counters
  , httpManager :: HTTP.Manager
  , prodproxyRuntime :: Maybe ProdProxy.Runtime
  }

initDevServerRuntime :: forall ext. Config -> Engine ext -> FilePath -> Tracer IO (DevServerTrack ext) -> IO (Runtime ext)
initDevServerRuntime cfg engine path devtracer = do
  rtCounters <- initCounters
  initialSite <- load rtCounters

  fsTVar <- newEmptyTMVarIO -- a swappable tmvar for debouncing
  siteTMVar <- newEmptyTMVarIO -- a swappable tmvar that doubles as a watch-trigger
  watches <- newTVarIO []

  notify <- FSNotify.startManager
  _ <- FSNotify.watchDir notify path shouldNotify (handleFSEvent fsTVar)
  _ <- forkIO (loadDebouncedFsEvents rtCounters fsTVar siteTMVar)

  proxyRuntime <- traverse initProxyBackend (api cfg)

  Runtime
    <$> pure (waitChanges watches)
    <*> pure (pushNewSite rtCounters siteTMVar)
    <*> pure devtracer
    <*> background (contramap adaptTracer devtracer) 0 initialSite (waitSiteAndNotify rtCounters siteTMVar watches)
    <*> pure rtCounters
    <*> newManager defaultManagerSettings
    <*> pure proxyRuntime
  where
    initProxyBackend :: (HostName, PortNum) -> IO ProdProxy.Runtime
    initProxyBackend (host,port) =
        ProdProxy.initRuntime (ProdProxy.StaticBackend (Text.encodeUtf8 host) port)

    load :: Counters -> IO (Site ext)
    load cntrs = do
      site <- execLoadSite engine
      Prometheus.setGauge (cnt_sources cntrs) (int2Double $ countSources $ site)
      pure site

    handleFSEvent :: TMVar () -> FSNotify.Event -> IO ()
    handleFSEvent x ev = do
      let modfile = getFilePath ev
      let shouldReload = (not . ignoreFileReload $ modfile)
      when shouldReload $ do
        runTracer devtracer $ FileWatch ev
        void $ atomically $ tryPutTMVar x ()

    loadDebouncedFsEvents :: Counters -> TMVar () -> TMVar (Site ext) -> IO ()
    loadDebouncedFsEvents cntrs x y = forever $ do
        atomically $ takeTMVar x
        -- give some time in case FileSystem is still synching
        threadDelay 100000
        loaded <- (Right <$> load cntrs) `catch` (\(e::SomeException) -> pure (Left e))
        case loaded of
          Right site -> do
            atomically $ do
              _ <- tryTakeTMVar y
              putTMVar y site
          Left e -> do
            runTracer devtracer $ SiteReloadException e

    pushNewSite :: Counters -> TMVar (Site ext) -> IO (Site ext, Bool)
    pushNewSite cntrs siteTMVar = do
      site <- load cntrs
      res <- seq site $ atomically $ tryPutTMVar siteTMVar site
      pure (site, res)

type WatchQueue = [TMVar ()]

-- Action waiting on a new Site and notifying a series of watches.
--
-- The action flushes the pending watches so that waiters do not have to remove
-- themselves from the queue.
waitSiteAndNotify :: Counters -> TMVar (Site ext) -> TVar WatchQueue -> Int -> IO ((Site ext), Int)
waitSiteAndNotify cntrs siteTMVar watches v = do
  Prometheus.incCounter $ cnt_reloads cntrs
  -- wait for siteTMVar and fan-out to all watches
  atomically $ do
    site <- takeTMVar siteTMVar
    old <- swapTVar watches []
    traverse_ (flip putTMVar ()) old
    pure (site, succ v)

-- | Enqueue oneself waiting for a new site.
waitChanges :: TVar WatchQueue -> IO ()
waitChanges watches = do
  w <- newEmptyTMVarIO
  -- append itself to watches
  atomically $ do
    modifyTVar' watches (\ws -> w:ws)
  void . atomically $ takeTMVar w

shouldNotify :: FSNotify.Event -> Bool
shouldNotify ev = case ev of
  FSNotify.Added _ _ _ -> True
  FSNotify.CloseWrite _ _ _ -> True
  FSNotify.Modified _ _ _ -> True
  FSNotify.ModifiedAttributes _ _ _ -> False
  FSNotify.Unknown _ _ _ _ -> False
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

ignoreFileReload :: FilePath -> Bool
ignoreFileReload p = takeExtension p == ".swp"

-- | Adapt the background-value tracks with a site by dropping the Site content
-- (which doesn't implement Show).
adaptTracer :: Background.Track (Site ext) -> DevServerTrack ext
adaptTracer trk = SiteReloaded $ fmap (const ()) trk


countSources :: Site ext -> Int
countSources s = sum
  [ length $ articles s
  , length $ images s
  , length $ videoFiles s
  , length $ cssFiles s
  , length $ jsFiles s
  , length $ htmlFiles s
  , length $ dotSourceFiles s
  , length $ rawFiles s
  ]
