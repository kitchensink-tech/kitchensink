{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KitchenSink.Layout.Blog.Summary (
    PathList (..),
    TargetType (..),
    PreambleSummary (..),
    summarizePreamble,
    TargetSummary (..),
    TopicSummary (..),
    summarizeTopic,
    GlossarySummary (..),
    GlossaryItem (..),
    summarizeGlossary,
    HashTagSummary (..),
    HashTagItem (..),
    summarizeHashTags,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import KitchenSink.Core.Section hiding (target)
import KitchenSink.Core.Section.Payloads qualified as SectionBasics
import KitchenSink.Layout.Blog.Analyses.ArticleInfos
import KitchenSink.Layout.Blog.Fragments
import KitchenSink.Prelude

data TargetType
    = CssTarget
    | ImageTarget
    | GraphVizImageTarget
    | VideoTarget
    | AudioTarget
    | RawTarget
    | DatasetTarget
    | DocumentTarget
    | JavaScriptSourceTarget
    | HtmlSourceTarget
    | JSONTarget
    | RootFileTarget
    | ArticleTarget
    | GeneratedTarget
    | TopicsIndexTarget
    | GlossaryTarget
    | HashTagsIndexTarget
    deriving (Show, Generic)
instance ToJSON TargetType
instance FromJSON TargetType

data PreambleSummary = PreambleSummary
    { author :: Text
    , datetxt :: Maybe Text
    , title :: Text
    , faviconUrl :: Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON PreambleSummary
instance ToJSON PreambleSummary

data TopicSummary = TopicSummary
    { topics :: [Text]
    , keywords :: [Text]
    , imageLink :: Maybe Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON TopicSummary
instance ToJSON TopicSummary

data GlossaryItem = GlossaryItem
    { term :: Text
    , definition :: Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON GlossaryItem
instance ToJSON GlossaryItem

data GlossarySummary = GlossarySummary
    { glossary :: [GlossaryItem]
    }
    deriving (Show, Eq, Generic)
instance FromJSON GlossarySummary
instance ToJSON GlossarySummary

data HashTagItem = HashTagItem
    { hashtag :: Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON HashTagItem
instance ToJSON HashTagItem

data HashTagSummary = HashTagSummary
    { hashtags :: [HashTagItem]
    }
    deriving (Show, Eq, Generic)
instance FromJSON HashTagSummary
instance ToJSON HashTagSummary

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

summarizeHashTags :: ArticleInfos -> HashTagSummary
summarizeHashTags infos = HashTagSummary (fmap f $ hashtagInfos infos)
  where
    f (HashTagInfo{hashtagValue}) = HashTagItem{hashtag = hashtagValue}

data TargetSummary
    = TargetSummary
    { targetType :: TargetType
    , textualTitle :: Maybe Text
    , textualSummary :: Maybe Text
    , preambleSummary :: Maybe PreambleSummary
    , topicSummary :: Maybe TopicSummary
    , glossarySummary :: Maybe GlossarySummary
    , hashtagSummary :: HashTagSummary
    }
    deriving (Show, Generic)
instance ToJSON TargetSummary
instance FromJSON TargetSummary

data PathList = PathList
    { paths :: [(Text, TargetSummary)]
    }
    deriving (Show, Generic)
instance ToJSON PathList
instance FromJSON PathList
