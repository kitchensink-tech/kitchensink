{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KitchenSink.Engine.MultiSite where

import Control.Concurrent.Async (Concurrently (..))
import Data.ByteString qualified as ByteString
import Data.Function ((&))
import Data.List (concatMap)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NEList
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS (newTlsManagerWith)
import Network.HTTP.ReverseProxy qualified as WaiProxy
import Network.HTTP.Types.Status (status404)
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Network.Wai.Middleware.RequestLogger qualified as RequestLogger
import Options.Generic
import Paths_prodapi qualified
import Prod.App qualified as Prod
import Prod.Proxy qualified as ProdProxy
import Prod.Proxy.MultiApp qualified as ProdProxy
import Prod.Status
import Prod.Tracer
import Prometheus qualified as Prometheus
import Servant
import Prelude (id)

import KitchenSink.Core.Build.Target (Target)
import KitchenSink.Engine.Config (ApiProxyConfig (..), Prefix, RewriteRule (..), SlashApiProxyDirective (..), TransportSecurity (..))
import KitchenSink.Engine.Counters (timeItWithLabel)
import KitchenSink.Engine.MultiSiteConfig
import KitchenSink.Engine.OnTheFly (OnTheFlyCounters (..), findTarget, handleOnTheFlyProduction)
import KitchenSink.Engine.Runtime (Engine (..))
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.SiteLoader qualified as SiteLoader
import KitchenSink.Engine.Track (DevServerTrack (..))
import KitchenSink.Engine.Utils
import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude

data Args
    = Args
    { configFile :: FilePath
    , variables :: [(Text, Text)]
    , httpPort :: Maybe Int
    , httpsPort :: Maybe Int
    , tlsKeyFile :: Maybe FilePath
    , tlsCertFile :: Maybe FilePath
    , proxyingTimeout :: Maybe Int
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
        <$> reg1c "ks_targets_requests" ("app", "status", "path") "number of queries per blog target"
        <*> reg1s "ks_ontheflybuild_time" ("app", "path") "time spent building targets on disk"
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
        (\(s, p) -> Prometheus.withLabel cntrs.count_Requests (a, s, p) Prometheus.incCounter)
        (\(p) work -> timeItWithLabel cntrs.duration_Build (a, p) work)

data Runtime
    = Runtime
    { counters :: Counters
    , httpManager :: Http.Manager
    , proxyCounters :: ProdProxy.Counters
    , vars :: [(Text, Text)]
    }

initRuntime :: Args -> IO Runtime
initRuntime args =
    Runtime
        <$> initCounters
        <*> newTlsManagerWith managersettings
        <*> ProdProxy.initCounters
        <*> pure args.variables
  where
    managersettings :: Http.ManagerSettings
    managersettings =
        Http.defaultManagerSettings
            & maybe id (\t setts -> setts{Http.managerResponseTimeout = Http.responseTimeoutMicro t}) timeout

    timeout :: Maybe Int
    timeout = args.proxyingTimeout

run :: Args -> IO ()
run cmd = do
    mcfg <- loadConfigFile @MultiSiteConfig cmd.configFile
    rt <- initRuntime cmd
    case mcfg of
        Nothing -> print ("could not load config app" :: Text)
        Just cfg -> do
            mfallback <- buildFallbackApp rt cfg
            mapps <- buildApplicationMap rt cfg
            mtls <- buildTLSMap cfg
            case (mfallback, mapps, mtls) of
                (_, _, Left err) -> print ("could not load config certs" :: Text, err)
                (Nothing, _, _) -> print ("could not setup fallback app" :: Text)
                (Just (_, fallbackApp), apps, Right creds) -> do
                    print $ Map.keys apps
                    tlsSettings <- loadTLSSettings creds
                    let multiapp = ProdProxy.routeApplication apps fallbackApp
                    healthRt <- Prod.alwaysReadyRuntime tracePrint
                    let status = pure ("ok" :: Text)
                    init <- Prod.initialize healthRt
                    let prodapiapp =
                            Prod.app
                                init
                                status
                                (statusPage <> versionsSection [("prodapi", Paths_prodapi.version)] <> metricsSection "js/metrics.js")
                                (coerce multiapp)
                                (Proxy @MultiSiteApi)
                    let webapp = RequestLogger.logStdoutDev prodapiapp
                    let httpWarp = Warp.run <$> cmd.httpPort <*> pure webapp
                    let httpsWarp = WarpTLS.runTLS <$> tlsSettings <*> tlsWarpSettings <*> pure webapp
                    let program = (,) <$> Concurrently (mio httpWarp) <*> Concurrently (mio httpsWarp)
                    void $ runConcurrently program
  where
    -- NOTE that there appear to be some repetition in the fallback of TLS certificates.
    -- We actually must provide two fallback:
    -- \* (a) one that warpTLS expects
    -- \* (b) one that ProdProxy.withTLSCredentialMap expects
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
        cert <- cmd.tlsCertFile
        key <- cmd.tlsKeyFile
        let base = WarpTLS.tlsSettings cert key
        pure $ ProdProxy.withTLSCredentialMap creds fallback base

    tlsWarpSettings :: Maybe Warp.Settings
    tlsWarpSettings = do
        port <- cmd.httpsPort
        pure $ Warp.setPort port Warp.defaultSettings

    loadDefaultCredentials :: IO (Maybe TLS.Credentials)
    loadDefaultCredentials = do
        let cert = cmd.tlsCertFile
        let key = cmd.tlsKeyFile
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
    fullApp Nothing Nothing =
        Nothing
    fullApp Nothing (Just y) =
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
buildProxyBackend rt cfg =
    case cfg of
        NoProxying -> pure Nothing
        (SlashApiProxyList []) -> pure Nothing
        (SlashApiProxy host port) ->
            pure $ Just $ mkProdProxyRuntime rt (static (Text.encodeUtf8 host) port)
        (SlashApiProxyList directives) -> do
            pure $ Just $ mkProdProxyRuntime rt (prefixed directives)
  where
    proxydest host port = WaiProxy.ProxyDest host port
    dest host port = WaiProxy.WPRProxyDest $ proxydest host port
    destSecure host port = WaiProxy.WPRProxyDestSecure $ proxydest host port
    mkdest directive =
        let
            host = Text.encodeUtf8 directive.hostname
         in
            case (directive.rewrite, directive.security) of
                (NoRewrite, UsePlainText) -> const $ dest host directive.portnum
                (NoRewrite, UseHTTPS) -> const $ destSecure host directive.portnum
                (DropPrefix, UsePlainText) ->
                    \req -> WaiProxy.WPRModifiedRequest (stripPrefix directive.prefix req) (proxydest host directive.portnum)
                (DropPrefix, UseHTTPS) ->
                    \req -> WaiProxy.WPRModifiedRequestSecure (stripPrefix directive.prefix req) (proxydest host directive.portnum)
                (RewritePrefix pfx, UsePlainText) ->
                    \req -> WaiProxy.WPRModifiedRequest (rewritePrefix directive.prefix pfx req) (proxydest host directive.portnum)
                (RewritePrefix pfx, UseHTTPS) ->
                    \req -> WaiProxy.WPRModifiedRequestSecure (rewritePrefix directive.prefix pfx req) (proxydest host directive.portnum)
                (RewritePrefixHost pfx newHost, UsePlainText) ->
                    \req -> WaiProxy.WPRModifiedRequest (changeHost (Text.encodeUtf8 newHost) $ rewritePrefix directive.prefix pfx $ req) (proxydest host directive.portnum)
                (RewritePrefixHost pfx newHost, UseHTTPS) ->
                    \req -> WaiProxy.WPRModifiedRequestSecure (changeHost (Text.encodeUtf8 newHost) $ rewritePrefix directive.prefix pfx $ req) (proxydest host directive.portnum)

    stripPrefix :: Prefix -> Wai.Request -> Wai.Request
    stripPrefix pfx req = req{Wai.rawPathInfo = ByteString.drop (ByteString.length $ Text.encodeUtf8 pfx) req.rawPathInfo}

    rewritePrefix :: Prefix -> Prefix -> Wai.Request -> Wai.Request
    rewritePrefix pfx newPfx req = req{Wai.rawPathInfo = (Text.encodeUtf8 newPfx) <> ByteString.drop (ByteString.length $ Text.encodeUtf8 pfx) req.rawPathInfo}

    changeHost :: ByteString.ByteString -> Wai.Request -> Wai.Request
    changeHost newHost req =
        let
            notHostHeader (hn, _) = hn /= "host"
         in
            req
                { Wai.requestHeaderHost = Just newHost
                , Wai.requestHeaders = ("host", newHost) : List.filter notHostHeader (Wai.requestHeaders req)
                }

    static host port =
        ProdProxy.WaiProxyBackend (\_ -> pure $ dest host port)
    prefixed directives =
        let
            matchPrefix req directive = Text.encodeUtf8 directive.prefix `ByteString.isPrefixOf` Wai.rawPathInfo req

            destinations = [(directive, mkdest directive) | directive <- directives]
            noDest = WaiProxy.WPRResponse $ Wai.responseLBS status404 [] "no such api route"
            findDestination req = fmap (\mk -> mk req) <$> List.find (matchPrefix req . fst) destinations
            getDestination req = maybe noDest snd (findDestination req)
         in
            ProdProxy.WaiProxyBackend (\req -> pure $ getDestination req)

buildDirectorySourceApp :: Runtime -> KitchenSinkDirectorySourceStanza -> SiteStanza -> IO Wai.Application
buildDirectorySourceApp rt src cfg = do
    -- loaded once for the whole duration of the application
    metadata <- loadMetadata src.metadata
    site <- loadSource
    let targets = evalTargets metadata site
    let engine = Engine (pure site) (pure metadata) (\_ _ -> targets) (produceTarget print)
    let webapp =
            handleOnTheFlyProduction
                (findTarget engine (pure site) tracePrint)
                (ontheflyCounters cfg.domain rt.counters)
                tracePrint
    pure webapp
  where
    -- An output prefix that is unused when generating results on the fly.
    unusedPrefix :: FilePath
    unusedPrefix = ""

    evalTargets :: MetaData -> SiteLoader.Site () -> [Target () ()]
    evalTargets med site =
        fmap (fmap $ const ())
            $ (siteTargets Blog.layout) (src.execRoot) unusedPrefix med site

    loadSource :: IO (SiteLoader.Site ())
    loadSource =
        SiteLoader.loadSite
            (fromMaybe "." src.dhallRoot)
            rt.vars
            (extraSectiontypes Blog.layout)
            (runTracer $ contramap Loading $ tracePrint)
            src.path

buildFallbackApp :: Runtime -> MultiSiteConfig -> IO (Maybe (TLS.HostName, Wai.Application))
buildFallbackApp rt cfg = case cfg.fallback of
    FallbackWithOminousError -> pure $ Just ("*", ominousApp)
    FallbackSite s ->
        let pickHeadHost (hosts, a) = (NEList.head hosts, a)
         in fmap pickHeadHost <$> buildSiteApplication rt s
  where
    ominousApp :: Wai.Application
    ominousApp = \_ reply -> reply $ Wai.responseLBS status404 [] "ominous error"

buildApplicationMap :: Runtime -> MultiSiteConfig -> IO ProdProxy.ApplicationMap
buildApplicationMap rt cfg =
    let
        flattenHosts (hosts, a) = [(host, a) | host <- NEList.toList hosts]
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
