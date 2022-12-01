{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine where

import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Lucid as Lucid
import qualified Lucid.Base as Lucid
import Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Options.Generic
import qualified Paths_prodapi
import Prelude (Read,error)
import Prod.Tracer
import Prod.App as Prod
import Prod.Status
import Servant
import System.FilePath.Posix ((</>))

import KitchenSink.Blog
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.SiteLoader as SiteLoader
import KitchenSink.Blog.Target hiding (Tracer)
import KitchenSink.Blog.Layout
import KitchenSink.Engine.Api
import KitchenSink.Engine.Config
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.Track (DevServerTrack(..))
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.Handlers

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

defaultMain :: IO ()
defaultMain = do
  cmd <- getRecord "kitchen-sink"
  let srcPath = coerce $ srcDir cmd
  let kitchensinkFilePath = srcPath </> "kitchen-sink.json"
  let portnum = coerce $ port cmd
  serveMetadata <- loadServeModeExtraData kitchensinkFilePath
  let prodengine = Engine
                  (loadSite (runTracer $ contramap Loading $ tracePrint) srcPath)
                  (pure serveMetadata)
                  (\med site -> fmap (fmap $ const ()) $ siteTargets (coerce $ outDir cmd) print med site)
                  (produceTarget print)
  let devengine = prodengine { execLoadMetaExtradata = loadDevModeExtraData kitchensinkFilePath }
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

loadServeModeExtraData :: FilePath -> IO MetaExtraData
loadServeModeExtraData path = do
  config <- fromMaybe defaultGlobalSite <$> loadJSONFile path
  MetaExtraData
    <$> getCurrentTime
    <*> pure (title config)
    <*> pure (publishURL config)
    <*> pure (twitterLogin config)
    <*> pure noExtraHeaders
    <*> pure (maybe [] (fmap baseURL) $ linkedSites config)
  where
    noExtraHeaders _ = pure mempty

loadDevModeExtraData :: FilePath -> IO MetaExtraData
loadDevModeExtraData path = do
  config <- fromMaybe defaultGlobalSite <$> loadJSONFile path
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
