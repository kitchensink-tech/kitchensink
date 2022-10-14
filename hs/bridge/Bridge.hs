{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Bridge where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import KitchenSink.Blog.Advanced (Node, TopicGraph, SkyLine, SkyLineItem, LinkInfo, ImageInfo)

data ArticleInfos = ArticleInfos {
    linkInfos :: [LinkInfo]
  , imageInfos :: [ImageInfo]
  , skyline :: SkyLine
  } deriving (Show, Generic)
instance ToJSON ArticleInfos
instance FromJSON ArticleInfos
