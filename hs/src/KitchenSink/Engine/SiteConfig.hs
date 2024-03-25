{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.SiteConfig where

import Data.Aeson (FromJSON, ToJSON)
import Dhall qualified
import GHC.Generics (Generic)

import KitchenSink.Prelude

data LinkedSite = LinkedSite
    { baseURL :: Text
    , siteType :: Text
    , siteTitle :: Text
    }
    deriving (Generic, Show)
instance FromJSON LinkedSite
instance ToJSON LinkedSite
instance Dhall.FromDhall LinkedSite

data SiteInfo = SiteInfo
    { title :: Text
    , publishURL :: Text
    , twitterLogin :: Maybe Text
    , linkedSites :: Maybe [LinkedSite]
    }
    deriving (Generic, Show)
instance FromJSON SiteInfo
instance ToJSON SiteInfo
instance Dhall.FromDhall SiteInfo

defaultSiteInfo :: SiteInfo
defaultSiteInfo =
    SiteInfo "invalid siteconfig!" "/" Nothing Nothing
