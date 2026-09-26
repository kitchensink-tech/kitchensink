module KitchenSink.Layout.Blog.Metadata (
    MetaData (..),
    HomeLinkSpec (..),
    defaultHomeLink,
    MenuItem (..),
    FooterColumn (..),
    FooterSpec (..),
    epochUTCTime,
) where

import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Lucid.Base qualified as Lucid

import KitchenSink.Layout.Blog.Extensions (Article, Assembler)
import KitchenSink.Prelude

data MetaData = MetaData
    { now :: UTCTime
    , baseTitle :: Text
    , publishBaseURL :: Text
    , twitterSiteLogin :: Maybe Text
    , extraHeaders :: Article [Text] -> Assembler (Lucid.Html ())
    , externalKitchenSinkURLs :: [Text]
    , pathPrefix :: Text
    , homeLinkSpec :: HomeLinkSpec
    , menuSpec :: [MenuItem]
    , footerSpec :: Maybe FooterSpec
    }

-- | How the top-left link back to the site root is rendered.
data HomeLinkSpec = HomeLinkSpec
    { homeLabel :: Text
    , homeIcon :: Maybe Text
    -- ^ image URL, relative to the site (the 'pathPrefix' is applied to a root-relative one)
    }

-- | An entry of the header menu (or a link of a footer column). URLs may be
-- root-relative, in which case the 'pathPrefix' is applied when rendering.
data MenuItem = MenuItem
    { menuLabel :: Text
    , menuUrl :: Text
    , menuChildren :: [MenuItem]
    }

data FooterColumn = FooterColumn
    { footerHeading :: Maybe Text
    , footerLinks :: [MenuItem]
    }

data FooterSpec = FooterSpec
    { footerColumns :: [FooterColumn]
    , footerLegal :: Maybe Text
    }

defaultHomeLink :: HomeLinkSpec
defaultHomeLink = HomeLinkSpec "Home" Nothing

epochUTCTime :: UTCTime
epochUTCTime = UTCTime (fromOrdinalDate 1970 1) (secondsToDiffTime 0)
