module KitchenSink.Layout.Blog.Metadata (
    MetaData (..),
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
    }

epochUTCTime :: UTCTime
epochUTCTime = UTCTime (fromOrdinalDate 1970 1) (secondsToDiffTime 0)
