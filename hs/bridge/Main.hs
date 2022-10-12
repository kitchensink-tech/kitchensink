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
import KitchenSink.Blog.Advanced (Node, TopicGraph, SkyLine, SkyLineItem, LinkInfo, ImageInfo, PathList)

import Bridge

data Bridge
  = PureScript
    { outputDir :: FilePath <?> "output directory"
    }
  deriving (Generic, Show)

instance ParseRecord Bridge


pureScriptTypes :: [SumType 'Bridge.Haskell]
pureScriptTypes =
  [ mkSumType (Proxy @(TopicGraph))
  , mkSumType (Proxy @Node)
  , mkSumType (Proxy @SkyLine)
  , mkSumType (Proxy @SkyLineItem)
  , mkSumType (Proxy @LinkInfo)
  , mkSumType (Proxy @ImageInfo)
  , mkSumType (Proxy @ArticleInfos)
  , mkSumType (Proxy @PathList)
  ]

main :: IO ()
main = do
  cmd <- getRecord "kitchen-sink-bridge"
  case cmd of
    PureScript path -> writePSTypes (unHelpful path) (buildBridge defaultBridge) pureScriptTypes
