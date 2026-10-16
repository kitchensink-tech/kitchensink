module Test.Main where

import Prelude

import Data.Array (elem)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)

import KitchenSink.Layout.Blog.Analyses.SiteGraph (TopicGraph(..))
import KitchenSink.Layout.Blog.Analyses.SiteGraph as KS
import Main (mergeExternalGraph)

siteA :: String
siteA = "https://a.example"

siteB :: String
siteB = "https://b.example"

local :: TopicGraph
local = TopicGraph
  { nodes:
      [ Tuple "topic:haskell" (KS.TopicNode "/topics/haskell.html" 1)
      , Tuple "#fp" (KS.HashTagNode "/hashtags/fp.html" 1)
      , Tuple "article:/post.html" (KS.ArticleNode "/post.html" 10)
      , Tuple ("site:" <> siteA) (KS.ExternalKitchenSinkSiteNode siteA)
      , Tuple ("site:" <> siteB) (KS.ExternalKitchenSinkSiteNode siteB)
      ]
  , edges: []
  }

-- | A remote site that shares the topic, the hashtag and (by key only) the
-- article of the local one.
remote :: TopicGraph
remote = TopicGraph
  { nodes:
      [ Tuple "topic:haskell" (KS.TopicNode "/topics/haskell.html" 2)
      , Tuple "topic:rust" (KS.TopicNode "/topics/rust.html" 1)
      , Tuple "#fp" (KS.HashTagNode "/hashtags/fp.html" 1)
      , Tuple "article:/post.html" (KS.ArticleNode "/post.html" 10)
      ]
  , edges: []
  }

edgesOf :: TopicGraph -> Array (Tuple String String)
edgesOf (TopicGraph g) = g.edges

assert :: String -> Boolean -> Effect Unit
assert what ok = if ok then log ("ok: " <> what) else throw ("failed: " <> what)

main :: Effect Unit
main = do
  let afterA = mergeExternalGraph [] siteA local remote
      nsA k = siteA <> "::" <> k
  assert "same topic linked to the local one" $
    Tuple "topic:haskell" (nsA "topic:haskell") `elem` edgesOf afterA
  assert "same hashtag linked to the local one" $
    Tuple "#fp" (nsA "#fp") `elem` edgesOf afterA
  for_ ["topic:rust", "article:/post.html"] \k ->
    assert ("no shared edge for " <> k) $
      not (Tuple k (nsA k) `elem` edgesOf afterA)

  let afterB = mergeExternalGraph [siteA] siteB afterA remote
      nsB k = siteB <> "::" <> k
  assert "topic on a third site linked to the local one" $
    Tuple "topic:haskell" (nsB "topic:haskell") `elem` edgesOf afterB
  assert "topic on a third site linked to the first merged site" $
    Tuple (nsA "topic:haskell") (nsB "topic:haskell") `elem` edgesOf afterB
  assert "rust links only across the two remote sites" $
    Tuple (nsA "topic:rust") (nsB "topic:rust") `elem` edgesOf afterB
      && not (Tuple "topic:rust" (nsB "topic:rust") `elem` edgesOf afterB)
