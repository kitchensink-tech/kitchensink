{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.SiteConfig where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Dhall qualified
import GHC.Generics (Generic)
import Prelude ((&&))

import KitchenSink.Layout.Blog.Metadata (FooterColumn (..), FooterSpec (..), HomeLinkSpec (..), MenuItem (..), defaultHomeLink)
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

-- | A plain link of the @menu@ or of a @footer@ column.
data NavLink = NavLink
    { label :: Text
    , url :: Text
    }
    deriving (Generic, Show)
instance FromJSON NavLink
instance ToJSON NavLink
instance Dhall.FromDhall NavLink

-- | An entry of the @menu@ array of @kitchen-sink.json@: a link, optionally
-- with a sub-menu of plain links (one level only).
data NavEntry = NavEntry
    { label :: Text
    , url :: Text
    , children :: Maybe [NavLink]
    }
    deriving (Generic, Show)
instance FromJSON NavEntry
instance ToJSON NavEntry
instance Dhall.FromDhall NavEntry

data FooterColumnConfig = FooterColumnConfig
    { heading :: Maybe Text
    , links :: [NavLink]
    }
    deriving (Generic, Show)
instance FromJSON FooterColumnConfig
instance ToJSON FooterColumnConfig
instance Dhall.FromDhall FooterColumnConfig

-- | The optional @footer@ object of @kitchen-sink.json@: columns of links and
-- a legal line.
data FooterConfig = FooterConfig
    { columns :: Maybe [FooterColumnConfig]
    , legal :: Maybe Text
    }
    deriving (Generic, Show)
instance FromJSON FooterConfig
instance ToJSON FooterConfig
instance Dhall.FromDhall FooterConfig

data SiteInfo = SiteInfo
    { title :: Text
    , publishURL :: Text
    , twitterLogin :: Maybe Text
    , linkedSites :: Maybe [LinkedSite]
    , basePath :: Maybe Text
    , homeLink :: Maybe HomeLink
    , menu :: Maybe [NavEntry]
    , footer :: Maybe FooterConfig
    }
    deriving (Generic, Show)
instance FromJSON SiteInfo
instance ToJSON SiteInfo
instance Dhall.FromDhall SiteInfo

defaultSiteInfo :: SiteInfo
defaultSiteInfo =
    SiteInfo "invalid siteconfig!" "/" Nothing Nothing Nothing Nothing Nothing Nothing

-- | The home link to render, falling back to the defaults for whatever the
-- configuration leaves out.
resolveHomeLink :: SiteInfo -> HomeLinkSpec
resolveHomeLink config =
    HomeLinkSpec
        (fromMaybe (homeLabel defaultHomeLink) (config.homeLink >>= (.label)))
        (config.homeLink >>= (.icon))

-- | The header menu to render: empty when the configuration has none.
resolveMenu :: SiteInfo -> [MenuItem]
resolveMenu config = fmap toItem (fromMaybe [] config.menu)
  where
    toItem e = MenuItem e.label e.url (fmap linkItem (fromMaybe [] e.children))

-- | The footer to render: 'Nothing' when the configuration has no columns and
-- no legal line.
resolveFooter :: SiteInfo -> Maybe FooterSpec
resolveFooter config = do
    f <- config.footer
    let cols = fmap toColumn (fromMaybe [] f.columns)
    if null cols && isNothing f.legal
        then Nothing
        else Just (FooterSpec cols f.legal)
  where
    toColumn c = FooterColumn c.heading (fmap linkItem c.links)

linkItem :: NavLink -> MenuItem
linkItem l = MenuItem l.label l.url []

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
