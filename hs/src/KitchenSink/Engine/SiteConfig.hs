{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.SiteConfig where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Dhall qualified
import GHC.Generics (Generic)

import KitchenSink.Layout.Blog.Metadata (HomeLinkSpec (..), defaultHomeLink)
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

-- | The optional @homeLink@ object of @kitchen-sink.json@: the label (default
-- @"Home"@) and an optional icon image of the link back to the site root.
data HomeLink = HomeLink
    { label :: Maybe Text
    , icon :: Maybe Text
    }
    deriving (Generic, Show)
instance FromJSON HomeLink
instance ToJSON HomeLink
instance Dhall.FromDhall HomeLink

data SiteInfo = SiteInfo
    { title :: Text
    , publishURL :: Text
    , twitterLogin :: Maybe Text
    , linkedSites :: Maybe [LinkedSite]
    , basePath :: Maybe Text
    , homeLink :: Maybe HomeLink
    }
    deriving (Generic, Show)
instance FromJSON SiteInfo
instance ToJSON SiteInfo
instance Dhall.FromDhall SiteInfo

defaultSiteInfo :: SiteInfo
defaultSiteInfo =
    SiteInfo "invalid siteconfig!" "/" Nothing Nothing Nothing Nothing

-- | The home link to render, falling back to the defaults for whatever the
-- configuration leaves out.
resolveHomeLink :: SiteInfo -> HomeLinkSpec
resolveHomeLink config =
    HomeLinkSpec
        (fromMaybe (homeLabel defaultHomeLink) (config.homeLink >>= (.label)))
        (config.homeLink >>= (.icon))

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
