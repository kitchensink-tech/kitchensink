
module KitchenSink.Engine.Runtime where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import GHC.Enum (succ)
import GHC.Float (int2Double)
import Network.HTTP.Client as HTTP (Manager, newManager, defaultManagerSettings)
import Prod.Tracer
import Prod.Background as Background
import qualified Prometheus as Prometheus
import qualified System.FSNotify as FSNotify
import System.FilePath.Posix (takeExtension)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.SiteLoader as SiteLoader
import KitchenSink.Blog.Target hiding (Tracer)
import KitchenSink.Blog.Layout
import KitchenSink.Engine.Counters (Counters(..), initCounters)
import KitchenSink.Engine.Track (DevServerTrack(..))

data Engine = Engine {
    execLoadSite :: IO Site
  , execLoadMetaExtradata :: IO MetaExtraData
  , evalTargets :: MetaExtraData -> Site -> [Target ()]
  , execProduceTarget :: Target () -> IO ()
  }

data Runtime = Runtime
  { waitReload :: IO ()
  , tryPushNewSite :: IO (Site, Bool)
  , traceDev :: Tracer IO DevServerTrack
  , liveSite :: BackgroundVal Site
  , counters :: Counters
  , httpManager :: HTTP.Manager
  }

initDevServerRuntime :: Engine -> FilePath -> Tracer IO DevServerTrack -> IO Runtime
initDevServerRuntime engine path devtracer = do
  rtCounters <- initCounters
  siteTMVar <- newEmptyTMVarIO
  watches <- newTVarIO []

  notify <- FSNotify.startManager
  _ <- FSNotify.watchDir notify path notifyPredicate (handleFSEvent rtCounters siteTMVar)
  
  Runtime
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
