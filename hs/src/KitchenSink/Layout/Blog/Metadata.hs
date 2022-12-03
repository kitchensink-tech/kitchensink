module KitchenSink.Layout.Blog.Metadata
  ( MetaData(..)
  , epochUTCTime
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import qualified Lucid.Base as Lucid

import KitchenSink.Core.Build.Site (Article)
import KitchenSink.Core.Build.Target (Assembler)
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
