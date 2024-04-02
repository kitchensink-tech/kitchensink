{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bridge where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import KitchenSink.Layout.Blog.Analyses (HashTagInfo, ImageInfo, LinkInfo, Node, SnippetInfo, TopicGraph)
import KitchenSink.Layout.Blog.Analyses.SkyLine (SkyLine, SkyLineItem)

data ArticleInfos = ArticleInfos
    { linkInfos :: [LinkInfo]
    , imageInfos :: [ImageInfo]
    , hashtagInfos :: [HashTagInfo]
    , snippetInfos :: [SnippetInfo]
    , skyline :: SkyLine
    }
    deriving (Show, Generic)
instance ToJSON ArticleInfos
instance FromJSON ArticleInfos
