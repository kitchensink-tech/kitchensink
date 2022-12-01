{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.SiteConfig where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import KitchenSink.Blog.Prelude

data LinkedSite = LinkedSite {
    baseURL :: Text
  , siteType :: Text
  , siteTitle :: Text
  } deriving (Generic, Show)
instance FromJSON LinkedSite

data GlobalSite = GlobalSite {
    title      :: Text
  , publishURL :: Text
  , twitterLogin :: Maybe Text
  , linkedSites :: Maybe [LinkedSite]
  } deriving (Generic, Show)
instance FromJSON GlobalSite

defaultGlobalSite :: GlobalSite
defaultGlobalSite =
  GlobalSite "invalid siteconfig!" "/" Nothing Nothing
