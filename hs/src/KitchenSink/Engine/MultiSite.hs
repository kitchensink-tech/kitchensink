{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module KitchenSink.Engine.MultiSite where

import Control.Concurrent.Async (Concurrently(..))
import Data.Maybe (catMaybes)
import Data.Time.Clock (getCurrentTime)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEList
import Data.List (concatMap)
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
import Prod.Tracer
import qualified Prod.App as Prod
import Prod.Status
import qualified Prod.Proxy.MultiApp as ProdProxy
import Servant

import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude
import KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.MultiSiteConfig
import KitchenSink.Engine.Track (DevServerTrack(..))
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.Handlers
import KitchenSink.Engine.Utils

data Args =
  Args
    { configFile :: FilePath <?> "dhall config file"
    , httpPort   :: Maybe Int <?> "port-num"
    , httpsPort   :: Maybe Int <?> "port-num"
    , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
    , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
    }
  deriving (Generic, Show)

type MultiSiteApi = Raw

run :: Args -> IO ()
run cmd = do
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
          init <- Prod.initialize healthRt
          let prodapiapp = Prod.app
               init
               status
               (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> metricsSection "js/metrics.js")
               (coerce multiapp)
               (Proxy @MultiSiteApi)
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
      FallbackSite s ->
        let pickHeadHost (hosts,a) = (NEList.head hosts,a)
        in fmap pickHeadHost <$> siteApplication s

    buildApplicationMap :: MultiSiteConfig -> IO ProdProxy.ApplicationMap
    buildApplicationMap cfg =
      let
        flattenHosts (hosts,a) = [(host,a) | host <- NEList.toList hosts]
        bundleApps = Map.fromList . concatMap flattenHosts
      in
      bundleApps . catMaybes <$> traverse siteApplication (sites cfg)

    siteApplication :: SiteStanza -> IO (Maybe (NEList.NonEmpty TLS.HostName, Wai.Application))
    siteApplication cfg = do
      case cfg.siteSource of
        NoFiles -> pure Nothing
        FileSource (FileSourceStanza path) -> do
          v <- fileSiteApp path cfg
          let k = Text.unpack cfg.domain
          let ks = fmap Text.unpack cfg.extraDomains
          pure $ Just (k :| ks, v)

    fileSiteApp:: FilePath -> SiteStanza -> IO Wai.Application
    fileSiteApp path cfg = do
      -- TODO: remove dependency on tmpDir here (initially as siteTargets outputdir)
      let evalTargets = \med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) (coerce cfg.tmpDir) med site
      let loadSource = loadSite (extraSectiontypes Blog.layout) (runTracer $ contramap Loading $ tracePrint) path
      -- loaded once for the whole duration
      metadata <- loadMetadata cfg.site
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

loadMetadata :: SiteInfo -> IO MetaData
loadMetadata config = do
  MetaData
    <$> getCurrentTime
    <*> pure (title config)
    <*> pure (publishURL config)
    <*> pure (twitterLogin config)
    <*> pure noExtraHeaders
    <*> pure (maybe [] (fmap baseURL) $ linkedSites config)
  where
    noExtraHeaders _ = pure mempty
