{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Options.Generic (ParseRecord, Generic, getRecord, type (<?>), unHelpful)
import Data.Proxy (Proxy(..))
import Data.Aeson (ToJSON,FromJSON)
import Language.PureScript.Bridge (writePSTypes, buildBridge, defaultBridge, mkSumType, SumType)
import qualified Language.PureScript.Bridge as Bridge

import KitchenSink.Layout.Blog.Analyses (Node, TopicGraph, HashTagInfo, LinkInfo, ImageInfo, SnippetInfo)
import KitchenSink.Layout.Blog.Analyses.SkyLine (SkyLine, SkyLineItem)
import KitchenSink.Layout.Blog.Summary (PathList, TargetSummary, TargetType, PreambleSummary, TopicSummary, GlossarySummary, GlossaryItem, HashTagSummary, HashTagItem)
import Bridge (ArticleInfos)

data Bridge
  = PureScript
    { outputDir :: FilePath <?> "output directory"
    }
  deriving (Generic, Show)

instance ParseRecord Bridge


pureScriptTypes :: [SumType 'Bridge.Haskell]
pureScriptTypes =
  [ mkSumType (Proxy @TopicGraph)
  , mkSumType (Proxy @Node)
  , mkSumType (Proxy @SkyLine)
  , mkSumType (Proxy @SkyLineItem)
  , mkSumType (Proxy @LinkInfo)
  , mkSumType (Proxy @HashTagInfo)
  , mkSumType (Proxy @ImageInfo)
  , mkSumType (Proxy @ArticleInfos)
  , mkSumType (Proxy @SnippetInfo)
  , mkSumType (Proxy @TargetSummary)
  , mkSumType (Proxy @GlossaryItem)
  , mkSumType (Proxy @GlossarySummary)
  , mkSumType (Proxy @HashTagItem)
  , mkSumType (Proxy @HashTagSummary)
  , mkSumType (Proxy @TargetType)
  , mkSumType (Proxy @PathList)
  , mkSumType (Proxy @PreambleSummary)
  , mkSumType (Proxy @TopicSummary)
  ]

main :: IO ()
main = do
  cmd <- getRecord "kitchen-sink-bridge"
  case cmd of
    PureScript path -> writePSTypes (unHelpful path) (buildBridge defaultBridge) pureScriptTypes
