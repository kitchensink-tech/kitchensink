{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module KitchenSink.Engine where

import Control.Concurrent.Async (Concurrently(..))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Time.Clock (getCurrentTime)
import Lucid as Lucid
import qualified Lucid.Base as Lucid
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Network.TLS as TLS
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Network.HTTP.Types.Status (status404)
import Options.Generic
import qualified Paths_prodapi
import Prelude (Read,error)
import Prod.Tracer
import Prod.App as Prod
import Prod.Status
import qualified Prod.Proxy.MultiApp as ProdProxy
import Servant
import System.FilePath.Posix ((</>))

import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude
import KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Core.Assembler (Assembler)
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.Api
import KitchenSink.Engine.Config
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.MultiSiteConfig
import KitchenSink.Engine.Track (DevServerTrack(..))
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.Handlers
import KitchenSink.Engine.Utils

data ServMode = SERVE | DEV
  deriving (Generic, Read, Show)

instance ParseField ServMode
instance ParseFields ServMode
instance ParseRecord ServMode

data Action
  = Produce
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
    }
  | Serve
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
    , servMode :: ServMode <?> "SERVE|DEV" 
    , httpPort   :: Maybe Int <?> "port-num"
    , httpsPort   :: Maybe Int <?> "port-num"
    , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
    , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
    }
  | MultiSite
    { configFile :: FilePath <?> "dhall config file"
    , httpPort   :: Maybe Int <?> "port-num"
    , httpsPort   :: Maybe Int <?> "port-num"
    , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
    , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
    }
  deriving (Generic, Show)

instance ParseRecord Action

ksPath
  :: FilePath <?> "source directory"
  -> Maybe FilePath <?> "kitchen-sink.json file"
  -> FilePath
ksPath base preferred =
  let fallback = coerce base </> "kitchen-sink.json"
  in fromMaybe fallback (coerce preferred)

defaultMain :: IO ()
defaultMain = do
  cmd <- getRecord "kitchen-sink"
  case cmd of
    Produce _ _ _ -> mainProduce cmd
    Serve _ _ _ _ _ _ _ _ -> mainServe cmd
    MultiSite _ _ _ _ _ -> mainMultiSite cmd

mainProduce :: Action -> IO ()
mainProduce cmd = do
  let srcPath = coerce $ srcDir cmd
  let kitchensinkFilePath = ksPath (srcDir cmd) (ksFile cmd)
  serveMetadata <- loadServeModeExtraData kitchensinkFilePath
  let prodengine = Engine
                  (loadSite (extraSectiontypes Blog.layout) (runTracer $ contramap Loading $ tracePrint) srcPath)
                  (pure serveMetadata)
                  (\med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) (coerce $ outDir cmd) med site)
                  (produceTarget print)
  case cmd of
    Produce _ _ _ -> do
      site <- execLoadSite prodengine
      meta <- execLoadMetaExtradata prodengine
      let tgts = evalTargets prodengine meta site
      traverse_ (execProduceTarget prodengine) tgts
    _ -> pure ()

mainServe :: Action -> IO ()
mainServe cmd = do
  let srcPath = coerce $ srcDir cmd
  let kitchensinkFilePath = ksPath (srcDir cmd) (ksFile cmd)
  serveMetadata <- loadServeModeExtraData kitchensinkFilePath
  let prodengine = Engine
                  (loadSite (extraSectiontypes Blog.layout) (runTracer $ contramap Loading $ tracePrint) srcPath)
                  (pure serveMetadata)
                  (\med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) (coerce $ outDir cmd) med site)
                  (produceTarget print)
  let devengine = prodengine { execLoadMetaExtradata = loadDevModeExtraData kitchensinkFilePath }
  case cmd of
    Serve _ _ _ mode _ _ _ _ -> do
      ksconfig <- loadJSONFile @Config kitchensinkFilePath >>= maybe (error "couldn't load kitchensink.json") pure
      kswebapp <- case (coerce mode) of
        DEV   -> runDev ksconfig devengine prodengine srcPath
        SERVE -> runServe ksconfig prodengine srcPath

      let webapp = RequestLogger.logStdoutDev kswebapp
      let httpWarp = Warp.run <$> coerce httpPort cmd <*> pure webapp
      let httpsWarp = WarpTLS.runTLS <$> tlsSettings <*> tlsWarpSettings <*> pure webapp

      let program = (,) <$> Concurrently (mio httpWarp) <*> Concurrently (mio httpsWarp)
      void $ runConcurrently program
    _ -> pure ()

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
      init <- initialize healthRt
      let webapp = app
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
      init <- initialize healthRt
      let webapp = app
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

mainMultiSite :: Action -> IO ()
mainMultiSite cmd = do
  mcfg <- loadConfigFile @MultiSiteConfig (coerce $ configFile cmd)
  case mcfg of
    Nothing -> print ("could not load config app" :: Text)
    Just cfg -> do
      mfallback <- buildFallbackApp cfg
      mapps <- buildApplicationMap cfg
      mtls <- buildTLSMap cfg
      case (mfallback,mapps,mtls) of
        (_,_,Left err) -> print ("could not load config certs" :: Text, err)
        (Nothing,_,_) -> print ("could not setup fallback app" :: Text)
        (Just (_,fallbackApp), apps, Right creds) -> do 
          print $ Map.keys apps
          defaultCreds <- loadDefaultCredentials
          let multiapp = ProdProxy.routeApplication apps fallbackApp
          healthRt <- Prod.alwaysReadyRuntime tracePrint
          let status = pure ("ok" :: Text)
          init <- initialize healthRt
          let prodapiapp = app
               init
               status
               (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> metricsSection "js/metrics.js")
               (coerce multiapp)
               (Proxy @Raw)
          let webapp = RequestLogger.logStdoutDev prodapiapp
          let httpWarp = Warp.run <$> coerce cmd.httpPort <*> pure webapp
          let httpsWarp = WarpTLS.runTLS <$> (tlsSettings creds =<< defaultCreds) <*> tlsWarpSettings <*> pure webapp
          let program = (,) <$> Concurrently (mio httpWarp) <*> Concurrently (mio httpsWarp)
          void $ runConcurrently program
  where
    ominousApp :: Wai.Application
    ominousApp = \_ reply -> reply $ Wai.responseLBS status404 [] "ominous error"

    buildFallbackApp :: MultiSiteConfig -> IO (Maybe (TLS.HostName, Wai.Application))
    buildFallbackApp cfg = case cfg.fallback of
      FallbackWithOminousError -> pure $ Just ("*", ominousApp)
      FallbackSite s -> siteApplication s

    buildApplicationMap :: MultiSiteConfig -> IO ProdProxy.ApplicationMap
    buildApplicationMap cfg =
      Map.fromList . catMaybes <$> traverse siteApplication (sites cfg)

    siteApplication :: SiteStanza -> IO (Maybe (TLS.HostName, Wai.Application))
    siteApplication cfg = do
      case cfg.siteSource of
        NoFiles -> pure Nothing
        FileSource (FileSourceStanza path) -> do
          v <- fileSiteApp path cfg
          let k = Text.unpack (domain cfg)
          pure $ Just (k, v)

    fileSiteApp:: FilePath -> SiteStanza -> IO Wai.Application
    fileSiteApp path cfg = do
      -- TODO: remove dependency on tmpDir here (initially as siteTargets outputdir)
      let evalTargets = \med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) (coerce cfg.tmpDir) med site
      let loadSource = loadSite (extraSectiontypes Blog.layout) (runTracer $ contramap Loading $ tracePrint) path
      -- loaded once for the whole duration
      metadata <- serveMetadataFromSiteInfo cfg.site
      site <- loadSource
      let engine = Engine (pure site) (pure metadata) evalTargets (produceTarget print)
      let webapp = handleOnTheFlyProduction2 (findTarget2 engine loadSource tracePrint) tracePrint
      pure webapp

    buildTLSMap :: MultiSiteConfig -> IO (Either String ProdProxy.CredentialMap)
    buildTLSMap cfg =
      ProdProxy.loadCredentialMap
        $ catMaybes
        $ fmap siteTLSTriplet (sites cfg)

    siteTLSTriplet :: SiteStanza -> Maybe (TLS.HostName, ProdProxy.X509Path, ProdProxy.PrivateKeyPath)
    siteTLSTriplet cfg = do
      c <- cfg.tls
      d <- c.sniDomain <|> pure cfg.domain
      pure (Text.unpack d, c.pem, c.key)

    tlsSettings :: ProdProxy.CredentialMap -> TLS.Credentials -> Maybe WarpTLS.TLSSettings
    tlsSettings creds fallback = do
       cert <- coerce tlsCertFile cmd
       key <- coerce tlsKeyFile cmd
       let base = WarpTLS.tlsSettings cert key
       pure $ ProdProxy.withTLSCredentialMap creds fallback base

    tlsWarpSettings :: Maybe Warp.Settings
    tlsWarpSettings = do
       port <- coerce httpsPort cmd
       pure $ Warp.setPort port Warp.defaultSettings

    loadDefaultCredentials :: IO (Maybe TLS.Credentials)
    loadDefaultCredentials = do 
       let cert = coerce tlsCertFile cmd
       let key = coerce tlsKeyFile cmd
       let go = TLS.credentialLoadX509 <$> cert <*> key
       case go of
         Nothing -> pure Nothing
         Just fetch -> do
           x <- fetch
           case x of
             Left _ -> pure $ Nothing
             Right c -> pure $ Just $ TLS.Credentials [c]

mio :: Maybe (IO ()) -> IO ()
mio = fromMaybe (pure ())
