{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge (SumType, buildBridge, defaultBridge, mkSumType, writePSTypes)
import qualified Language.PureScript.Bridge as Bridge
import Options.Generic (Generic, ParseRecord, getRecord, unHelpful, type (<?>))

import Bridge (ArticleInfos)
import KitchenSink.Layout.Blog.Analyses (HashTagInfo, ImageInfo, LinkInfo, Node, SnippetInfo, TopicGraph)
import KitchenSink.Layout.Blog.Analyses.SkyLine (SkyLine, SkyLineItem)
import KitchenSink.Layout.Blog.Summary (GlossaryItem, GlossarySummary, HashTagItem, HashTagSummary, PathList, PreambleSummary, TargetSummary, TargetType, TopicSummary)

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
