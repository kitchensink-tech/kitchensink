{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module KitchenSink.Engine.MultiSite where

import Control.Concurrent.Async (Concurrently(..))
import Data.Maybe (catMaybes)
import Data.Time.Clock (getCurrentTime)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NEList
import qualified Data.List as List
import Data.List (concatMap)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
import Network.HTTP.Client as HTTP (Manager, newManager, defaultManagerSettings)
import qualified Prod.Proxy as ProdProxy
import qualified Prod.Proxy.MultiApp as ProdProxy
import Servant
import qualified Prometheus as Prometheus

import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude
import qualified KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Core.Build.Target (Target)
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.Config (SlashApiProxyDirective(..),ApiProxyConfig(..))
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.MultiSiteConfig
import KitchenSink.Engine.Track (DevServerTrack(..))
import KitchenSink.Engine.Runtime (Engine(..))
import KitchenSink.Engine.OnTheFly (OnTheFlyCounters(..), handleOnTheFlyProduction, findTarget)
import KitchenSink.Engine.Counters (timeItWithLabel)
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

data Counters
  = Counters
  { count_Requests :: Prometheus.Vector (Text, Text, Text) Prometheus.Counter
  , duration_Build :: Prometheus.Vector (Text, Text) Prometheus.Summary
  }

initCounters :: IO Counters
initCounters =
  Counters
    <$> reg1c "ks_targets_requests" ("app","status","path") "number of queries per blog target"
    <*> reg1s "ks_ontheflybuild_time" ("app","path") "time spent building targets on disk"
  where
    reg1c k t h =
      Prometheus.register
        $ Prometheus.vector t
        $ Prometheus.counter (Prometheus.Info k h)
    reg1s k t h =
      Prometheus.register
        $ Prometheus.vector t
        $ Prometheus.summary (Prometheus.Info k h) Prometheus.defaultQuantiles

ontheflyCounters :: HostName -> Counters -> OnTheFlyCounters a
ontheflyCounters a cntrs = 
  OnTheFlyCounters
    (\(s,p) -> Prometheus.withLabel cntrs.count_Requests (a,s,p) Prometheus.incCounter)
    (\(p) work -> timeItWithLabel cntrs.duration_Build (a,p) work)

data Runtime
  = Runtime
  { counters :: Counters
  , httpManager :: HTTP.Manager
  , proxyCounters :: ProdProxy.Counters
  }

initRuntime :: IO Runtime
initRuntime =
  Runtime
    <$> initCounters
    <*> newManager defaultManagerSettings
    <*> ProdProxy.initCounters

run :: Args -> IO ()
run cmd = do
  mcfg <- loadConfigFile @MultiSiteConfig (coerce $ configFile cmd)
  rt <- initRuntime
  case mcfg of
    Nothing -> print ("could not load config app" :: Text)
    Just cfg -> do
      mfallback <- buildFallbackApp rt cfg
      mapps <- buildApplicationMap rt cfg
      mtls <- buildTLSMap cfg
      case (mfallback,mapps,mtls) of
        (_,_,Left err) -> print ("could not load config certs" :: Text, err)
        (Nothing,_,_) -> print ("could not setup fallback app" :: Text)
        (Just (_,fallbackApp), apps, Right creds) -> do 
          print $ Map.keys apps
          tlsSettings <- loadTLSSettings creds
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
          let httpsWarp = WarpTLS.runTLS <$> tlsSettings <*> tlsWarpSettings <*> pure webapp
          let program = (,) <$> Concurrently (mio httpWarp) <*> Concurrently (mio httpsWarp)
          void $ runConcurrently program
  where
    -- NOTE that there appear to be some repetition in the fallback of TLS certificates.
    -- We actually must provide two fallback:
    -- * (a) one that warpTLS expects
    -- * (b) one that ProdProxy.withTLSCredentialMap expects
    -- The credential (b) serves as default when SNI fails to find a hostname.
    -- The credential (a) serves as default when there is no SNI indication.
    -- Today these two situations are conflated and if you end up needing to
    -- separate the two for any reason, let me know.
    loadTLSSettings :: ProdProxy.CredentialMap -> IO (Maybe WarpTLS.TLSSettings)
    loadTLSSettings creds = do
       fallback <- loadDefaultCredentials
       pure $ bundleTLSSettings creds =<< fallback

    bundleTLSSettings :: ProdProxy.CredentialMap -> TLS.Credentials -> Maybe WarpTLS.TLSSettings
    bundleTLSSettings creds fallback = do
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

type SiteOnlyApi = Raw
type ProxyOnlyApi = Raw
type SiteAndProxyApi =
  ("api" :> ProxyOnlyApi) :<|> SiteOnlyApi

buildSiteApplication :: Runtime -> SiteStanza -> IO (Maybe (NEList.NonEmpty TLS.HostName, Wai.Application))
buildSiteApplication rt cfg = do
  proxyApp <- fmap ProdProxy.handleProxy <$> buildProxyBackend rt cfg.api
  ksApp <- case cfg.site of
    NoFiles -> pure Nothing
    KitchenSinkDirectorySource src ->
      Just <$> buildDirectorySourceApp rt src cfg
  pure $ wrap <$> fullApp proxyApp ksApp
  where
    fullApp :: Maybe Wai.Application -> Maybe Wai.Application -> Maybe Wai.Application
    fullApp  Nothing Nothing =
      Nothing
    fullApp  Nothing (Just y) =
      Just $ Servant.serve (Proxy @SiteOnlyApi) (coerce y)
    fullApp (Just x) Nothing =
      Just $ Servant.serve (Proxy @ProxyOnlyApi) (coerce x)
    fullApp (Just x) (Just y) =
      Just $ Servant.serve (Proxy @SiteAndProxyApi) ((coerce x) :<|> (coerce y))

    wrap :: Wai.Application -> (NEList.NonEmpty TLS.HostName, Wai.Application)
    wrap v = do
      let k = Text.unpack cfg.domain
      let ks = fmap Text.unpack cfg.extraDomains
      (k :| ks, v)

-- | TODO: modify ProdProxy to use keyed counters somehow
mkProdProxyRuntime :: Runtime -> ProdProxy.Backends -> ProdProxy.Runtime
mkProdProxyRuntime rt backends =
  ProdProxy.Runtime
    (rt.proxyCounters)
    (backends)
    (rt.httpManager)

buildProxyBackend :: Runtime -> ApiProxyConfig -> IO (Maybe ProdProxy.Runtime)
buildProxyBackend _ NoProxying = pure Nothing
buildProxyBackend rt (SlashApiProxy host port) =
    pure $ Just $ mkProdProxyRuntime rt (ProdProxy.StaticBackend (Text.encodeUtf8 host) port)
buildProxyBackend _ (SlashApiProxyList []) = pure Nothing
buildProxyBackend rt (SlashApiProxyList triplets) = do
    let adapt directive = (Text.encodeUtf8 directive.hostname, directive.portnum)
    let hasreqprefix req directive = Text.encodeUtf8 directive.prefix `ByteString.isPrefixOf` Wai.rawPathInfo req
    let flookup req = pure $ fmap adapt $ List.find (hasreqprefix req) triplets
    pure $ Just (mkProdProxyRuntime rt (ProdProxy.DynamicBackend flookup))


buildDirectorySourceApp:: Runtime -> KitchenSinkDirectorySourceStanza -> SiteStanza -> IO Wai.Application
buildDirectorySourceApp rt src cfg = do
    -- loaded once for the whole duration of the application
    metadata <- loadMetadata src.metadata
    site <- loadSource
    let targets = evalTargets metadata site
    let engine = Engine (pure site) (pure metadata) (\_ _ -> targets) (produceTarget print)
    let webapp = handleOnTheFlyProduction
                   (findTarget engine (pure site) tracePrint)
                   (ontheflyCounters cfg.domain rt.counters)
                   tracePrint
    pure webapp
  where
    -- An output prefix that is unused when generating results on the fly.
    unusedPrefix :: FilePath
    unusedPrefix = ""

    evalTargets :: MetaData -> SiteLoader.Site () -> [Target () ()]
    evalTargets med site = fmap (fmap $ const ())
      $ (siteTargets Blog.layout) unusedPrefix med site

    loadSource :: IO (SiteLoader.Site ())
    loadSource = SiteLoader.loadSite (extraSectiontypes Blog.layout) (runTracer $ contramap Loading $ tracePrint) src.path

buildFallbackApp :: Runtime -> MultiSiteConfig -> IO (Maybe (TLS.HostName, Wai.Application))
buildFallbackApp rt cfg = case cfg.fallback of
  FallbackWithOminousError -> pure $ Just ("*", ominousApp)
  FallbackSite s ->
    let pickHeadHost (hosts,a) = (NEList.head hosts,a)
    in fmap pickHeadHost <$> buildSiteApplication rt s
  where
    ominousApp :: Wai.Application
    ominousApp = \_ reply -> reply $ Wai.responseLBS status404 [] "ominous error"

buildApplicationMap :: Runtime -> MultiSiteConfig -> IO ProdProxy.ApplicationMap
buildApplicationMap rt cfg =
  let
    flattenHosts (hosts,a) = [(host,a) | host <- NEList.toList hosts]
    bundleApps = Map.fromList . concatMap flattenHosts
  in
  bundleApps . catMaybes <$> traverse (buildSiteApplication rt) cfg.services

buildTLSMap :: MultiSiteConfig -> IO (Either String ProdProxy.CredentialMap)
buildTLSMap cfg =
  ProdProxy.loadCredentialMap
    $ mconcat
    $ fmap siteTLSTriplet cfg.services

siteTLSTriplet :: SiteStanza -> [(TLS.HostName, ProdProxy.X509Path, ProdProxy.PrivateKeyPath)]
siteTLSTriplet cfg = do
  t <- cfg.tls
  c <- toList (tlsCert t)
  d <- join (toList (t.sniDomains <|> (Just [cfg.domain])))
  pure (Text.unpack d, c.pem, c.key)
  where
    tlsCert :: TLSStanza -> Maybe CertificateFiles
    tlsCert s = case s.certificate of
                  NoCertificates -> Nothing
                  CertificateFileSource x -> Just x

