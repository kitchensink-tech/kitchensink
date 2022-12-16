{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module KitchenSink.Layout.Blog.Analyses.TopicStats
  ( Tag
  , TopicStats(..)
  , buildTopicStats
  , allTags
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import KitchenSink.Layout.Blog.Extensions (Target, Assembler, Article)
import KitchenSink.Core.Build.Target (Sourced(..), runAssembler)
import KitchenSink.Core.Section
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections
import KitchenSink.Layout.Blog.Destinations

data TopicStats = TopicStats {
    byTopic      :: Map Tag [(Target (), Article [Text])]
  , knownTargets :: [(Target (), Article [Text])]
  }

allTags :: TopicStats -> [Tag]
allTags = Map.keys . byTopic

buildTopicStats :: [Sourced (Article [Text])] -> (Sourced (Article [Text]) -> Target ()) -> TopicStats
buildTopicStats arts mkTarget =
    TopicStats indexByTopic [(mkTarget sa, a) | sa@(Sourced _ a) <- arts]
  where
    indexByTopic = Map.fromListWith (<>)
      $ [ (tag, [(mkTarget s, art)])
      | s@(Sourced _ art) <- arts
      , tag <- getTags art
      ]

    getTags :: Article [Text] -> [Tag]
    getTags art = either (const []) tags . runAssembler $ (f art)

    f :: Article [Text] -> Assembler TopicData
    f art = extract <$> json @() @TopicData art Topic
