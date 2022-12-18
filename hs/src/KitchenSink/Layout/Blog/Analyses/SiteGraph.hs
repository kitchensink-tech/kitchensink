{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module KitchenSink.Layout.Blog.Analyses.SiteGraph
  ( TopicGraph(..)
  , topicsgraph
  , Node(..)
  , ExternalSitesInfo(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import GHC.Generics (Generic)
import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import qualified Data.Text as Text

import KitchenSink.Layout.Blog.Extensions (Target)
import KitchenSink.Core.Build.Target (destination, destinationUrl)
import KitchenSink.Prelude
import KitchenSink.Layout.Blog.Destinations

import KitchenSink.Layout.Blog.Analyses.TopicStats
import KitchenSink.Layout.Blog.Analyses.ArticleInfos
import KitchenSink.Layout.Blog.Analyses.SkyLine

type NodeKey = Text

type URL = Text

data Node
  = TopicNode URL Int
  | ArticleNode URL Int
  | ImageNode URL
  | ExternalKitchenSinkSiteNode URL
  deriving (Generic, Show)
instance ToJSON Node
instance FromJSON Node

data TopicGraph = TopicGraph {
    nodes :: [(NodeKey, Node)]
  , edges :: [(NodeKey,NodeKey)]
  }
  deriving (Generic, Show)
instance ToJSON TopicGraph
instance FromJSON TopicGraph

data ExternalSitesInfo = ExternalSitesInfo {
    externalKitchenSinks :: [URL]
}

topicsgraph :: ExternalSitesInfo -> TopicStats -> TopicGraph
topicsgraph external stats =
    TopicGraph
      (topicNodes <> articleNodes <> imagesNodes <> externalKSSitesNodes)
      (topicArticleEdges <> articleArticleEdges <> articleImageEdges <> articleExternalSiteEdges)
  where
    topicNodes,articleNodes,externalKSSitesNodes :: [(NodeKey, Node)]
    topicNodes = [ (topicKey t, TopicNode (destinationUrl $ destTopic "" t) (length xs)) | (t,xs) <- Map.toList (byTopic stats) ]
    articleNodes = [ (articleKey t, ArticleNode (targetUrl t) histsize) | (t,histsize) <- uniqueTargetArticles ]
    imagesNodes = [ (imageKey url, ImageNode url) | url <- uniqueImages ]
    externalKSSitesNodes = [ (externalSiteKey url, ExternalKitchenSinkSiteNode url) | url <- externalKitchenSinks external]

    topicArticleEdges :: [(NodeKey, NodeKey)]
    topicArticleEdges =
        mconcat
        $ fmap (\(topic, xs) -> [ (topicKey topic, articleKey tgt) | (tgt,_) <- xs])
        $ Map.toList (byTopic stats)

    allLinks :: [(Target (), LinkInfo)]
    allLinks = do
      -- list monad!
      (from, a) <- knownTargets stats
      link <- linkInfos $ analyzeArticle a
      pure (from,link)

    articleArticleEdges :: [(NodeKey, NodeKey)]
    articleArticleEdges = do
      -- list monad!
      (from, link) <- allLinks
      toKey <- maybe [] (:[]) (lookupArticleLink link)
      pure (articleKey from, toKey)

    articleExternalSiteEdges :: [(NodeKey, NodeKey)]
    articleExternalSiteEdges = do
      -- list monad!
      (from, link) <- allLinks
      toKey <- maybe [] (:[]) (lookupExternalSiteLink link)
      pure (articleKey from, toKey)

    articleImageEdges :: [(NodeKey, NodeKey)]
    articleImageEdges = do
      -- list monad!
      ((from, _), infos) <- List.zip (knownTargets stats) analyses
      img <- imageInfos $ infos
      pure (articleKey from, imageKey $ imageURL img)

    topicKey t = "topic:" <> t
    articleKey t = "article:" <> targetUrl t
    imageKey t = "image:" <> t
    externalSiteKey t = "site:" <> t

    targetUrl t = destinationUrl (destination t)

    uniqueTargetArticles :: [(Target (),Int)]
    uniqueTargetArticles = do
      ((from, _), infos) <- List.zip (knownTargets stats) analyses
      pure (from, articleWeight infos)

    articleWeight :: ArticleInfos -> Int
    articleWeight info = sum
      $ fmap skylineItemWeight
      $ skylineItems
      $ skyline info

    skylineItemWeight :: SkyLineItem -> Int
    skylineItemWeight x = case x of
      HeaderMark _ _ -> 30
      ImageMark _ _ -> 10
      TextualMark w _ -> w

    analyses :: [ArticleInfos]
    analyses = fmap (analyzeArticle . snd) $ knownTargets stats

    uniqueImages :: [Text]
    uniqueImages = List.nub $ do
       infos <- analyses
       img <- imageInfos $ infos
       pure $ imageURL img

    lookupArticleLink :: LinkInfo -> Maybe NodeKey
    lookupArticleLink (LinkInfo url _) =
      fmap articleKey
      $ List.find (\t -> targetUrl t == url)
      $ fmap fst
      $ knownTargets stats

    lookupExternalSiteLink :: LinkInfo -> Maybe NodeKey
    lookupExternalSiteLink (LinkInfo url _) =
      fmap externalSiteKey
      $ List.find (\t -> t `Text.isPrefixOf` url)
      $ externalKitchenSinks external
