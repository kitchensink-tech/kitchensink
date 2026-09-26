module KitchenSink.Layout.Blog.Metadata (
    MetaData (..),
    HomeLinkSpec (..),
    defaultHomeLink,
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
    }

-- | How the top-left link back to the site root is rendered.
data HomeLinkSpec = HomeLinkSpec
    { homeLabel :: Text
    , homeIcon :: Maybe Text
    -- ^ image URL, relative to the site (the 'pathPrefix' is applied to a root-relative one)
    }

defaultHomeLink :: HomeLinkSpec
defaultHomeLink = HomeLinkSpec "Home" Nothing

epochUTCTime :: UTCTime
epochUTCTime = UTCTime (fromOrdinalDate 1970 1) (secondsToDiffTime 0)
