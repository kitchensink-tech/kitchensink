{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module KitchenSink.Layout.Blog.Analyses.TopicStats
  ( TopicStats(..)
  , buildTopicStats
  , allTopicNames
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import KitchenSink.Layout.Blog.Analyses.ArticleInfos (analyzeArticle, hashtagInfos, HashTagInfo)
import KitchenSink.Layout.Blog.Extensions (Target, Assembler, Article)
import KitchenSink.Core.Build.Target (Sourced(..), runAssembler)
import KitchenSink.Core.Section
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections

type TagValue = HashTagInfo

data TopicStats = TopicStats {
    byTopic      :: Map TopicName [(Target (), Article [Text])]
  , byHashTag    :: Map TagValue [(Target (), Article [Text])]
  , knownTargets :: [(Target (), Article [Text])]
  }

allTopicNames :: TopicStats -> [TopicName]
allTopicNames = Map.keys . byTopic

buildTopicStats :: [Sourced (Article [Text])] -> (Sourced (Article [Text]) -> Target ()) -> TopicStats
buildTopicStats arts mkTarget =
    TopicStats indexByTopic indexByHashTag [(mkTarget sa, a) | sa@(Sourced _ a) <- arts]
  where
    indexByTopic = Map.fromListWith (<>)
      $ [ (topic, [(mkTarget s, art)])
      | s@(Sourced _ art) <- arts
      , topic <- getTopicNames art
      ]

    indexByHashTag = Map.fromListWith (<>)
      $ [ (tag, [(mkTarget s, art)])
      | s@(Sourced _ art) <- arts
      , tag <- getHashtagNames art
      ]

    getTopicNames :: Article [Text] -> [TopicName]
    getTopicNames art = either (const []) topics . runAssembler $ (topicSection art)

    topicSection :: Article [Text] -> Assembler TopicData
    topicSection art = extract <$> json @() @TopicData art Topic

    getHashtagNames :: Article [Text] -> [HashTagInfo]
    getHashtagNames art = hashtagInfos $ analyzeArticle art

