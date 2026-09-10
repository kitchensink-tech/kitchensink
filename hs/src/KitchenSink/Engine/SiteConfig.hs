{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.SiteConfig where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as Text
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
    , basePath :: Maybe Text
    }
    deriving (Generic, Show)
instance FromJSON SiteInfo
instance ToJSON SiteInfo
instance Dhall.FromDhall SiteInfo

defaultSiteInfo :: SiteInfo
defaultSiteInfo =
    SiteInfo "invalid siteconfig!" "/" Nothing Nothing Nothing

-- | Normalizes the configured @basePath@ (e.g., @Just "tramaj"@, @Just
-- "/tramaj/"@, @Nothing@) into a prefix ready to prepend to the
-- root-relative URLs generated throughout the site: no trailing slash, a
-- single leading slash when non-empty, and @""@ when hosted at the domain
-- root. This is what makes a GitHub Pages *project* page (served under
-- @https:\/\/user.github.io\/reponame\/@ rather than the domain root) work.
normalizedBasePath :: SiteInfo -> Text
normalizedBasePath config =
    case Text.dropWhileEnd (== '/') . dropLeadingSlashes <$> basePath config of
        Nothing -> ""
        Just "" -> ""
        Just p -> "/" <> p
  where
    dropLeadingSlashes = Text.dropWhile (== '/')
