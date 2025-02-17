{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KitchenSink.Engine.Produce where

import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Prod.Tracer

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
    , outDir :: FilePath
    , ksFile :: Maybe FilePath
    , variables :: [(Text, Text)]
    }

run :: Args -> IO ()
run cmd = do
    let srcPath = cmd.srcDir
    let kitchensinkFilePath = kitshenSinkJsonFilePath cmd.srcDir cmd.ksFile
    serveMetadata <- loadMetadata kitchensinkFilePath
    let handleLoadSite =
            loadSite
                "."
                cmd.variables
                (extraSectiontypes Blog.layout)
                (runTracer $ contramap Loading $ tracePrint)
                srcPath
    let prodengine =
            Engine
                (handleLoadSite)
                (pure serveMetadata)
                (\med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) Nothing cmd.outDir med site)
                (produceTarget print)
    site <- execLoadSite prodengine
    meta <- execLoadMetaExtradata prodengine
    let tgts = evalTargets prodengine meta site
    traverse_ (execProduceTarget prodengine) tgts

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
  where
    noExtraHeaders _ = pure mempty
