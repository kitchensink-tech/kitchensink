{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module KitchenSink.Engine.Serve where

import Control.Concurrent.Async (Concurrently(..))
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Lucid as Lucid
import qualified Lucid.Base as Lucid
import Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Options.Generic
import qualified Paths_prodapi
import Prelude (Read,error)
import Prod.Tracer
import qualified Prod.App as Prod
import Prod.Status
import Servant

import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude
import KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Core.Assembler (Assembler)
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.Api
import KitchenSink.Engine.Config
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.Track (DevServerTrack(..))
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.Handlers
import KitchenSink.Engine.Utils

data ServMode = SERVE | DEV
  deriving (Generic, Read, Show)

instance ParseField ServMode
instance ParseFields ServMode
instance ParseRecord ServMode

data Args
  = Args
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
    , servMode :: ServMode <?> "SERVE|DEV" 
    , httpPort   :: Maybe Int <?> "port-num"
    , httpsPort   :: Maybe Int <?> "port-num"
    , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
    , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
    }

run :: Args -> IO ()
run cmd = do
  let srcPath = coerce $ srcDir cmd
  let kitchensinkFilePath = ksPath (srcDir cmd) (ksFile cmd)
  serveMetadata <- loadServeModeExtraData kitchensinkFilePath
  let prodengine = Engine
                  (loadSite "." (extraSectiontypes Blog.layout) (runTracer $ contramap Loading $ tracePrint) srcPath)
                  (pure serveMetadata)
                  (\med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) (coerce $ outDir cmd) med site)
                  (produceTarget print)
  let devengine = prodengine { execLoadMetaExtradata = loadDevModeExtraData kitchensinkFilePath }
  ksconfig <- loadJSONFile @Config kitchensinkFilePath >>= maybe (error "couldn't load kitchensink.json") pure
  kswebapp <- case (coerce cmd.servMode) of
    DEV   -> runDev ksconfig devengine prodengine srcPath
    SERVE -> runServe ksconfig prodengine srcPath

  let webapp = RequestLogger.logStdoutDev kswebapp
  let httpWarp = Warp.run <$> coerce httpPort cmd <*> pure webapp
  let httpsWarp = WarpTLS.runTLS <$> tlsSettings <*> tlsWarpSettings <*> pure webapp

  let program = (,) <$> Concurrently (mio httpWarp) <*> Concurrently (mio httpsWarp)
  void $ runConcurrently program

  where

    tlsSettings :: Maybe WarpTLS.TLSSettings
    tlsSettings = do
       cert <- coerce tlsCertFile cmd
       key <- coerce tlsKeyFile cmd
       pure $ WarpTLS.tlsSettings cert key

    tlsWarpSettings :: Maybe Warp.Settings
    tlsWarpSettings = do
       port <- coerce httpsPort cmd
       pure $ Warp.setPort port Warp.defaultSettings

    runDev ksconfig devengine prodengine path = do
      let apiStatus = pure ("ok" :: Text)
      healthRt <- Prod.alwaysReadyRuntime tracePrint
      rt <- initDevServerRuntime ksconfig devengine path tracePrint
      init <- Prod.initialize healthRt
      let webapp = Prod.app
           init
           apiStatus
           (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> metricsSection "js/metrics.js")
           (serveDevApi ksconfig devengine prodengine rt)
           (Proxy @DevApi)
      pure webapp

    runServe ksconfig engine path = do
      let apiStatus = pure ("ok" :: Text)
      healthRt <- Prod.alwaysReadyRuntime tracePrint
      rt <- initDevServerRuntime ksconfig engine path tracePrint
      init <- Prod.initialize healthRt
      let webapp = Prod.app
           init
           apiStatus
           (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> metricsSection "js/metrics.js")
           (serveApi engine rt)
           (Proxy @ServeApi)
      pure webapp

loadServeModeExtraData :: FilePath -> IO MetaData
loadServeModeExtraData path = do
  config <- fromMaybe defaultSiteInfo <$> loadJSONFile @SiteInfo path
  serveMetadataFromSiteInfo config

serveMetadataFromSiteInfo :: SiteInfo -> IO MetaData
serveMetadataFromSiteInfo config = do
  MetaData
    <$> getCurrentTime
    <*> pure (title config)
    <*> pure (publishURL config)
    <*> pure (twitterLogin config)
    <*> pure noExtraHeaders
    <*> pure (maybe [] (fmap baseURL) $ linkedSites config)
  where
    noExtraHeaders _ = pure mempty

loadDevModeExtraData :: FilePath -> IO MetaData
loadDevModeExtraData path = do
  config <- fromMaybe defaultSiteInfo <$> loadJSONFile @SiteInfo path
  MetaData
    <$> getCurrentTime
    <*> pure (title config)
    <*> pure (publishURL config)
    <*> pure (twitterLogin config)
    <*> pure jsReloadExtraHeaders
    <*> pure (maybe [] (fmap baseURL) $ linkedSites config)
  where
    jsReloadExtraHeaders :: Article ext [Text] -> Assembler ext (Lucid.Html ())
    jsReloadExtraHeaders _ =
      let js1 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/autoreload.js" ] ""
          js2 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/add-dev-route.js" ] ""
          js3 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/echarts.min.js" ] ""
          js5 = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ "/js/echart-histogram.js" ] ""
      in pure (js1 *> js2 *> js3 *> js5)
