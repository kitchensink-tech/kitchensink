{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KitchenSink.Engine.Serve where

import Control.Concurrent.Async (Concurrently (..))
import Control.Monad (when)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (getCurrentTime)
import Lucid as Lucid
import Lucid.Base qualified as Lucid
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Network.Wai.Middleware.RequestLogger qualified as RequestLogger
import Options.Generic
import Paths_prodapi_core qualified
import Prod.App qualified as Prod
import Prod.Background (readBackgroundVal)
import Prod.Status
import Prod.Tracer
import Servant
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Prelude (Read, error, (&&), (||))

import KitchenSink.Core.Assembler (Assembler)
import KitchenSink.Engine.Acme (AcmeOptions, acmeTlsSettings, runAcme, setupAcmeOrExit)
import KitchenSink.Engine.Api
import KitchenSink.Engine.Config
import KitchenSink.Engine.Diagnostics (reportDiagnostics)
import KitchenSink.Engine.Dynamic (dynamicMiddleware, dynamicOptionsFromConfig)
import KitchenSink.Engine.Handlers
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Engine.Track (DevServerTrack (..))
import KitchenSink.Engine.Utils
import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude

data ServMode = SERVE | DEV
    deriving (Generic, Read, Show)

instance ParseField ServMode
instance ParseFields ServMode
instance ParseRecord ServMode

data Args
    = Args
    { srcDir :: FilePath
    , outDir :: Maybe FilePath
    , ksFile :: Maybe FilePath
    , servMode :: ServMode
    , variables :: [(Text, Text)]
    , httpPort :: Maybe Int
    , httpsPort :: Maybe Int
    , tlsKeyFile :: Maybe FilePath
    , tlsCertFile :: Maybe FilePath
    , dynamic :: Bool
    , acmeDomains :: [Text]
    -- ^ when non-empty, certificates for these domains come from ACME (tls-alpn-01)
    , acmeOptions :: AcmeOptions
    }

run :: Args -> IO ()
run cmd = do
    when (not (null cmd.acmeDomains) && (isJust cmd.tlsCertFile || isJust cmd.tlsKeyFile)) $ do
        hPutStrLn stderr "--acmeDomain and --tlsCertFile/--tlsKeyFile are exclusive: certificates come either from ACME or from files"
        exitFailure
    acme <- traverse (setupAcmeOrExit cmd.acmeOptions) (NonEmpty.nonEmpty cmd.acmeDomains)
    let srcPath = cmd.srcDir
    let kitchensinkFilePath = kitshenSinkJsonFilePath cmd.srcDir cmd.ksFile
    serveMetadata <- loadServeModeExtraData kitchensinkFilePath
    let adaptTargets = fmap (fmap (const ()))
    let handleLoadSite = do
            site <-
                loadSite
                    cmd.variables
                    serveMetadata.pathPrefix
                    (extraSectiontypes Blog.layout)
                    (runTracer $ contramap Loading $ tracePrint)
                    srcPath
            -- also on every reload, so an edit that introduces a problem shows up
            reportDiagnostics (siteDiagnostics Blog.layout site)
            pure site
    let prodengine =
            Engine
                handleLoadSite
                (pure serveMetadata)
                (\med site -> adaptTargets (siteTargets Blog.layout Nothing (fromMaybe "./out" cmd.outDir) med site))
                (produceTarget print)
    let devengine = prodengine{execLoadMetaExtradata = loadDevModeExtraData kitchensinkFilePath}
    ksconfig <- loadJSONFile @Config kitchensinkFilePath >>= maybe (error "couldn't load kitchensink.json") pure
    kswebapp <- case cmd.servMode of
        DEV -> runDev ksconfig devengine srcPath
        SERVE -> runServe ksconfig prodengine srcPath

    let webapp = RequestLogger.logStdoutDev kswebapp
    let httpWarp = Warp.run <$> cmd.httpPort <*> pure webapp
    -- with ACME, the HTTPS server defaults to port 443: the only one the
    -- ACME server connects to for tls-alpn-01 challenges
    let httpsPort = cmd.httpsPort <|> fmap (const 443) acme
    let tlsSettings = fmap acmeTlsSettings acme <|> fileTlsSettings
    let tlsWarpSettings = fmap (\port -> Warp.setPort port Warp.defaultSettings) httpsPort
    let httpsWarp = WarpTLS.runTLS <$> tlsSettings <*> tlsWarpSettings <*> pure webapp
    let renewer = fmap runAcme acme

    let program =
            (,,)
                <$> Concurrently (mio httpWarp)
                <*> Concurrently (mio httpsWarp)
                <*> Concurrently (mio renewer)
    void $ runConcurrently program
  where
    fileTlsSettings :: Maybe WarpTLS.TLSSettings
    fileTlsSettings = do
        cert <- cmd.tlsCertFile
        key <- cmd.tlsKeyFile
        pure $ WarpTLS.tlsSettings cert key

    runDev ksconfig devengine path = do
        let apiStatus = pure ("ok" :: Text)
        healthRt <- Prod.alwaysReadyRuntime tracePrint
        rt <- initDevServerRuntime ksconfig devengine path tracePrint
        init <- Prod.initialize healthRt
        let webapp =
                Prod.app
                    init
                    apiStatus
                    (statusPage <> versionsSection [("prodapi", Paths_prodapi_core.version)] <> metricsSection "js/metrics.js")
                    (serveDevApi devengine rt)
                    (Proxy @DevApi)
        pure $ withDynamicRouting ksconfig rt webapp

    runServe ksconfig engine path = do
        let apiStatus = pure ("ok" :: Text)
        healthRt <- Prod.alwaysReadyRuntime tracePrint
        rt <- initDevServerRuntime ksconfig engine path tracePrint
        init <- Prod.initialize healthRt
        let webapp =
                Prod.app
                    init
                    apiStatus
                    (statusPage <> versionsSection [("prodapi", Paths_prodapi_core.version)] <> metricsSection "js/metrics.js")
                    (serveApi engine rt)
                    (Proxy @ServeApi)
        pure $ withDynamicRouting ksconfig rt webapp

    -- opt-in (--dynamic): tries every request-time dynamic route before
    -- falling back to the site's ordinary (static / on-the-fly) production;
    -- see "KitchenSink.Engine.Dynamic"
    withDynamicRouting ksconfig rt webapp =
        dynamicMiddleware (dynamicOptionsFromConfig cmd.dynamic ksconfig) (readBackgroundVal (liveSite rt)) webapp

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
        <*> pure (normalizedBasePath config)
        <*> pure (resolveHomeLink config)
        <*> pure (resolveMenu config)
        <*> pure (resolveFooter config)
  where
    noExtraHeaders _ = pure mempty

loadDevModeExtraData :: FilePath -> IO MetaData
loadDevModeExtraData path = do
    config <- fromMaybe defaultSiteInfo <$> loadJSONFile @SiteInfo path
    let prefix = normalizedBasePath config
    MetaData
        <$> getCurrentTime
        <*> pure (title config)
        <*> pure (publishURL config)
        <*> pure (twitterLogin config)
        <*> pure (jsReloadExtraHeaders prefix)
        <*> pure (maybe [] (fmap baseURL) $ linkedSites config)
        <*> pure prefix
        <*> pure (resolveHomeLink config)
        <*> pure (resolveMenu config)
        <*> pure (resolveFooter config)
  where
    jsReloadExtraHeaders :: Text -> Article ext [Text] -> Assembler ext (Lucid.Html ())
    jsReloadExtraHeaders prefix _ =
        let js1 = Lucid.termRawWith "script" [type_ "text/javascript", src_ (prefix <> "/js/autoreload.js")] ""
            js3 = Lucid.termRawWith "script" [type_ "text/javascript", src_ (prefix <> "/js/echarts.min.js")] ""
            js5 = Lucid.termRawWith "script" [type_ "text/javascript", src_ (prefix <> "/js/echart-histogram.js")] ""
         in pure (js1 *> js3 *> js5)
