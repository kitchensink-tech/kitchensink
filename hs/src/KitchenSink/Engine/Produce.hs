{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KitchenSink.Engine.Produce where

import Control.Exception (SomeAsyncException (..), SomeException, displayException, fromException, throwIO, try)
import Control.Monad (when)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Prod.Tracer
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Prelude ((||))

import KitchenSink.Core.Build.Target (Target, destination, destinationUrl)
import KitchenSink.Engine.Diagnostics (hasFailures, reportDiagnostics)
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Engine.Track (DevServerTrack (..))
import KitchenSink.Engine.Utils
import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude

data Args
    = Args
    { srcDir :: FilePath
    , outDir :: Maybe FilePath
    , ksFile :: Maybe FilePath
    , variables :: [(Text, Text)]
    , abortOnError :: Bool
    -- ^ stop at the first target that fails to produce (default: report it, keep producing the rest, exit non-zero at the end)
    }

run :: Args -> IO ()
run cmd = do
    -- progress goes to stdout and problems to stderr: keep their lines whole when both land in one log
    hSetBuffering stdout LineBuffering
    let srcPath = cmd.srcDir
    let kitchensinkFilePath = kitshenSinkJsonFilePath cmd.srcDir cmd.ksFile
    serveMetadata <- loadMetadata kitchensinkFilePath
    let handleLoadSite =
            loadSite
                cmd.variables
                serveMetadata.pathPrefix
                (extraSectiontypes Blog.layout)
                (runTracer $ contramap Loading $ tracePrint)
                srcPath
    let prodengine =
            Engine
                (handleLoadSite)
                (pure serveMetadata)
                (\med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) Nothing (fromMaybe "./out" cmd.outDir) med site)
                (produceTarget print)
    site <- execLoadSite prodengine
    meta <- execLoadMetaExtradata prodengine
    let diagnostics = siteDiagnostics Blog.layout site
    reportDiagnostics diagnostics
    let tgts = evalTargets prodengine meta site
    failedTargets <-
        if cmd.abortOnError
            then traverse_ (execProduceTarget prodengine) tgts >> pure []
            else catMaybes <$> traverse (produceOrReport prodengine) tgts
    -- everything that could be produced has been; still fail the command
    when (hasFailures diagnostics || not (null failedTargets)) $ do
        when (not (null failedTargets)) $
            hPutStrLn stderr (show (length failedTargets) <> " target(s) failed to produce; use --abortOnError to stop at the first one")
        exitFailure

-- | Produces one target; a failure is reported on stderr (and returned) instead of ending the run.
produceOrReport :: Engine () -> Target () () -> IO (Maybe Text)
produceOrReport engine tgt = do
    r <- try @SomeException (execProduceTarget engine tgt)
    case r of
        Right () -> pure Nothing
        Left e
            | Just (SomeAsyncException _) <- fromException e -> throwIO e
            | otherwise -> do
                let url = destinationUrl (destination tgt)
                hPutStrLn stderr (Text.unpack url <> ": error: " <> displayException e)
                pure (Just url)

loadMetadata :: FilePath -> IO MetaData
loadMetadata path = do
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
  where
    noExtraHeaders _ = pure mempty
