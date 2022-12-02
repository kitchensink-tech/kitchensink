module KitchenSink.Blog.Layout.Metadata
  ( MetaExtraData(..)
  , epochUTCTime
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import qualified Lucid.Base as Lucid

import KitchenSink.Blog.Build.Site (Article)
import KitchenSink.Blog.Build.Target (Assembler)
import KitchenSink.Blog.Prelude

data MetaExtraData = MetaExtraData
  { now :: UTCTime
  , baseTitle :: Text
  , publishBaseURL :: Text
  , twitterSiteLogin :: Maybe Text
  , extraHeaders :: Article [Text] -> Assembler (Lucid.Html ())
  , externalKitchenSinkURLs :: [Text]
  }

epochUTCTime :: UTCTime
epochUTCTime = UTCTime (fromOrdinalDate 1970 1) (secondsToDiffTime 0)
