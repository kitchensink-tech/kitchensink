{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module KitchenSink.Layout.Blog.Analyses.TopicStats (
    TopicStats (..),
    buildTopicStats,
    allTopicNames,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import KitchenSink.Core.Assembler.Sections
import KitchenSink.Core.Build.Target (Sourced (..), runAssembler)
import KitchenSink.Core.Section
import KitchenSink.Layout.Blog.Analyses.ArticleInfos (HashTagInfo, analyzeArticle, hashtagInfos)
import KitchenSink.Layout.Blog.ArticleTypes (isArticleLikeLayout, layoutNameFor)
import KitchenSink.Layout.Blog.Extensions (Article, Assembler, Target)
import KitchenSink.Prelude

type TagValue = HashTagInfo

data TopicStats = TopicStats
    { byTopic :: Map TopicName [(Target (), Article [Text])]
    , byHashTag :: Map TagValue [(Target (), Article [Text])]
    , knownTargets :: [(Target (), Article [Text])]
    }

allTopicNames :: TopicStats -> [TopicName]
allTopicNames = Map.keys . byTopic

-- | Topics and hashtags index the articles only: a page of the site itself
-- (home page, landing page, application...) is not listed under its topics,
-- same rule as the article listings and the feeds. 'knownTargets' has
-- every page.
buildTopicStats :: [Sourced (Article [Text])] -> (Sourced (Article [Text]) -> Target ()) -> TopicStats
buildTopicStats arts mkTarget =
    TopicStats indexByTopic indexByHashTag [(mkTarget sa, a) | sa@(Sourced _ a) <- arts]
  where
    indexByTopic =
        Map.fromListWith (<>)
            $ [ (top, [(mkTarget s, art)])
              | s@(Sourced _ art) <- arts
              , isArticleLikeLayout (layoutNameFor art)
              , top <- getTopicNames art
              ]

    indexByHashTag =
        Map.fromListWith (<>)
            $ [ (tag, [(mkTarget s, art)])
              | s@(Sourced _ art) <- arts
              , isArticleLikeLayout (layoutNameFor art)
              , tag <- getHashtagNames art
              ]

    getTopicNames :: Article [Text] -> [TopicName]
    getTopicNames art = either (const []) topics . runAssembler $ (topicSection art)

    topicSection :: Article [Text] -> Assembler TopicData
    topicSection art = extract <$> json @() @TopicData art isTopic

    getHashtagNames :: Article [Text] -> [HashTagInfo]
    getHashtagNames art = hashtagInfos $ analyzeArticle art
