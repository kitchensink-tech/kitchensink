{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module KitchenSink.Engine.Produce where

import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Options.Generic
import Prod.Tracer

import KitchenSink.Layout.Blog as Blog
import KitchenSink.Prelude
import KitchenSink.Engine.SiteLoader as SiteLoader
import KitchenSink.Engine.SiteBuilder (produceTarget)
import KitchenSink.Engine.SiteConfig
import KitchenSink.Engine.Track (DevServerTrack(..))
import KitchenSink.Engine.Runtime
import KitchenSink.Engine.Utils

data Args
  = Args
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
    }

run :: Args -> IO ()
run cmd = do
  let srcPath = coerce $ srcDir cmd
  let kitchensinkFilePath = ksPath (srcDir cmd) (ksFile cmd)
  serveMetadata <- loadMetadata kitchensinkFilePath
  let prodengine = Engine
                  (loadSite "." (extraSectiontypes Blog.layout) (runTracer $ contramap Loading $ tracePrint) srcPath)
                  (pure serveMetadata)
                  (\med site -> fmap (fmap $ const ()) $ (siteTargets Blog.layout) (coerce $ outDir cmd) med site)
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

