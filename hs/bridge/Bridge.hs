{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Bridge where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import KitchenSink.Layout.Blog.Analyses (Node, TopicGraph, LinkInfo, ImageInfo, SnippetInfo)
import KitchenSink.Layout.Blog.Analyses.SkyLine (SkyLine, SkyLineItem)

data ArticleInfos = ArticleInfos {
    linkInfos :: [LinkInfo]
  , imageInfos :: [ImageInfo]
  , snippetInfos :: [SnippetInfo]
  , skyline :: SkyLine
  } deriving (Show, Generic)
instance ToJSON ArticleInfos
instance FromJSON ArticleInfos
