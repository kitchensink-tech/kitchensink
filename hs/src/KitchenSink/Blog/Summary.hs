{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module KitchenSink.Blog.Summary (
    PathList(..)
  , TargetType(..)
  , PreambleSummary(..)
  , summarizePreamble
  , TargetSummary(..)
  , TopicSummary(..)
  , summarizeTopic
  , GlossarySummary(..)
  , summarizeGlossary
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Data.Text (Text)

import KitchenSink.Core.Section hiding (target)
import qualified KitchenSink.Core.Section.Payloads as SectionBasics
import KitchenSink.Prelude
import KitchenSink.Blog.Fragments

data TargetType
  = CssTarget
  | ImageTarget
  | GraphVizImageTarget
  | VideoTarget
  | RawTarget
  | JavaScriptSourceTarget
  | HtmlSourceTarget
  | JSONTarget
  | RootFileTarget
  | ArticleTarget
  | GeneratedTarget
  | TopicsIndexTarget
  deriving (Show, Generic)
instance ToJSON TargetType
instance FromJSON TargetType

data PreambleSummary = PreambleSummary {
    author  :: Text
  , datetxt :: Maybe Text
  , title   :: Text
  , faviconUrl   :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON PreambleSummary
instance ToJSON PreambleSummary

data TopicSummary = TopicSummary {
    tags :: [Text]
  , keywords :: [Text]
  , imageLink :: Maybe Text
  } deriving (Show, Eq, Generic)
instance FromJSON TopicSummary
instance ToJSON TopicSummary

data GlossaryItem = GlossaryItem {
    term :: Text
  , definition :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON GlossaryItem
instance ToJSON GlossaryItem

data GlossarySummary = GlossarySummary {
    glossary :: [GlossaryItem]
  } deriving (Show, Eq, Generic)
instance FromJSON GlossarySummary
instance ToJSON GlossarySummary

summarizePreamble :: SectionBasics.PreambleData -> PreambleSummary
summarizePreamble p =
  PreambleSummary
    (SectionBasics.author p)
    (SectionBasics.datetxt p)
    (SectionBasics.title p)
    (fromMaybe defaultFavicon $ SectionBasics.faviconUrl p)

summarizeTopic :: TopicData -> TopicSummary
summarizeTopic (TopicData{..}) = TopicSummary{..}

summarizeGlossary :: GlossaryData -> GlossarySummary
summarizeGlossary (GlossaryData terms) = GlossarySummary (fmap f terms)
  where
    f (GlossaryTerm{..}) = GlossaryItem{..} 

data TargetSummary
  = TargetSummary
  { targetType :: TargetType
  , textualTitle :: Maybe Text
  , textualSummary :: Maybe Text
  , preambleSummary :: Maybe PreambleSummary
  , topicSummary :: Maybe TopicSummary
  , glossarySummary :: Maybe GlossarySummary
  } deriving (Show, Generic)
instance ToJSON TargetSummary
instance FromJSON TargetSummary

data PathList = PathList {
    paths :: [(Text, TargetSummary)]
  }
  deriving (Show, Generic)
instance ToJSON PathList
instance FromJSON PathList
