
module KSGraph
 ( chartOptions
 , Options
 , Legend
 , Series
 , Event
 , Category(..)
 , decodeEvent
 , ClickedItem(..)
 , NodeId
 , Edge
 , Node
 , module Control.Monad.Except
 ) where

import Prelude
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Tuple (Tuple(..))
import Data.List.NonEmpty (NonEmptyList, singleton)
import Foreign (Foreign, ForeignError(..), readArray, readBoolean, readInt, readString, readNullOrUndefined)
import Foreign.Index ((!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Array (concat, elem)
import Data.Lens (view, toArrayOf, traversed, to, filtered)
import Data.Lens.Fold (anyOf)
import Data.Number as Number
import Data.String as String
import Data.String.Pattern (Pattern(..))

import KitchenSink.Layout.Blog.Analyses.SiteGraph (TopicGraph, _TopicGraph)
import KitchenSink.Layout.Blog.Analyses.SiteGraph as KS

import Halogen.ECharts as ECharts

type Legend = { data :: Array String }

type Event =
  ( name :: String
  , dataType :: String
  , data :: Foreign
  )

decodeEvent :: Record Event -> Except (NonEmptyList ForeignError) ClickedItem
decodeEvent event
  | event.dataType == "edge" = ClickedEdge <$> decodeEdge event.data
  | event.dataType == "node" = ClickedNode <$> decodeNode event.data
  | otherwise                = pure $ UnsupportedDataType

decodeNode :: Foreign -> Except (NonEmptyList ForeignError) Node
decodeNode value = do
  id <- value ! "id" >>= readString
  name <- value ! "name" >>= readString
  category <- value ! "category" >>= readInt >>= readCategory
  pure {id, name, category}

decodeEdge :: Foreign -> Except (NonEmptyList ForeignError) Edge
decodeEdge value = do
  target <- value ! "target" >>= readString
  source <- value ! "source" >>= readString
  pure {target, source}

data Category
  = Articles
  | Topics
  | HashTags
  | Images
  | ExternalSites

derive instance eqCategory :: Eq Category

readCategory :: Int -> Except (NonEmptyList ForeignError) Category
readCategory = case _ of
  0 -> pure Articles
  1 -> pure Topics
  2 -> pure HashTags
  3 -> pure Images
  4 -> pure ExternalSites
  _ -> throwError $ singleton (ForeignError "Unsupported category")

type NodeId = String

type Node = { id :: NodeId, name :: String, category :: Category }

type Edge = { source :: NodeId, target :: NodeId }

data ClickedItem
  = UnsupportedDataType
  | ClickedNode Node
  | ClickedEdge Edge

type Series =
  { name :: String
  , type :: String
  , layout :: String
  , data :: Array
      { id :: String
      , name :: String
      , category :: Int
      , symbol :: String
      , symbolSize :: Number
      , itemStyle :: { opacity :: Number }
      , label :: { show :: Boolean }
      }
  , links :: Array { source :: String, target:: String }
  , categories :: Array { name::String }
  , roam :: Boolean
  , draggable :: Boolean
  , label :: { position :: String }
  , lineStyle :: { opacity :: Number }
  , force :: { repulsion :: Int }
  }

type Options =
  ( legend :: Array Legend
  , series :: Array Series
  )

-- | `expandedExternalSites` lists the URLs of external kitchen-sink sites
-- whose own `topicsgraph.json` has already been fetched and merged into
-- `graph` (see `Main.purs`'s click-to-expand handling). It only drives the
-- node's rendering (a filled circle once expanded vs. a hollow triangle
-- before); the actual fetch+merge state lives in `Main`'s component state.
--
-- `keyword` highlights the nodes whose key or URL contains it (ignoring
-- case): they are enlarged and labelled, every other node and edge is
-- faded. An empty (or blank) keyword is no search: the normal rendering.
-- Article titles are not part of `topicsgraph.json`, so they can't be
-- searched.
chartOptions :: TopicGraph -> Maybe Node -> Array String -> String -> ECharts.Input Options
chartOptions graph focusedNode expandedExternalSites keyword =
  let
    needle :: String
    needle = String.toLower (String.trim keyword)

    searching :: Boolean
    searching = needle /= ""

    isMatch :: String -> String -> Boolean
    isMatch key name =
      searching && (contains key || contains name)
      where
      contains s = String.contains (Pattern needle) (String.toLower s)

    -- Opacity and label of a node with this key and name.
    highlight :: String -> String -> { itemStyle :: { opacity :: Number }, label :: { show :: Boolean } }
    highlight key name
      | isMatch key name = { itemStyle: { opacity: 1.0 }, label: { show: true } }
      | searching = { itemStyle: { opacity: 0.15 }, label: { show: false } }
      | otherwise = { itemStyle: { opacity: 1.0 }, label: { show: false } }

    isSelection :: String -> Boolean
    isSelection n1id = maybe false (\n2 -> n2.id == n1id) focusedNode
    
    hasUndirectedEdge :: String -> String -> Boolean
    hasUndirectedEdge n1 n2 =
      let match (Tuple m1 m2) = (n1 == m1 && n2 == m2) || (n2 == m1 && n1 == m2)
      in
      anyOf (_TopicGraph <<< to _.edges <<< traversed) match graph

    isSelectionNeighbor :: String -> Boolean
    isSelectionNeighbor n1id = maybe false (\n2 -> hasUndirectedEdge n1id n2.id) focusedNode
    
    imageSymbol :: String -> String -> String
    imageSymbol url n1id
      | isSelection n1id || isSelectionNeighbor n1id || isMatch n1id url = "image://" <> url
      | otherwise = "circle"

    -- `name` is only used to tell search matches, which are drawn at
    -- least at the size of a selection neighbour.
    selectionSize :: String -> String -> Number -> Number
    selectionSize n1id name default
      | isSelection n1id = 35.0
      | isSelectionNeighbor n1id = 30.0
      | otherwise = searchSize n1id name default

    searchSize :: String -> String -> Number -> Number
    searchSize n1id name default
      | isMatch n1id name = max 20.0 default
      | otherwise = default

    articleSize :: Int -> String -> Number
    -- `n` is 0 for generated pages (glossary, topics and hashtags
    -- listings), and `log 0` is `-Infinity`. A single non-finite
    -- `symbolSize` makes the bounding rect of the whole graph NaN, and
    -- echarts only roams when the pointer is inside that rect: one such
    -- node silently disables pan and zoom for the whole graph.
    articleSize n _ = 10.0 + Number.log(toNumber (max 1 n)) / Number.log(2.0)

    topicSize :: String -> Number
    topicSize _ = 10.0

    hashtagSize :: String -> Number
    hashtagSize _ = 8.0

    imageSize :: String -> Number
    imageSize _ = 5.0

    externalSiteSize :: String -> Number
    externalSiteSize _ = 15.0

    echartNode (Tuple key node) =
      let
        n = baseNode key node
        h = highlight key n.name
      in
        { id: n.id
        , name: n.name
        , category: n.category
        , symbol: n.symbol
        , symbolSize: n.symbolSize
        , itemStyle: h.itemStyle
        , label: h.label
        }

    baseNode key node = case node of
      KS.ArticleNode url n ->
        { id: key
        , name: url
        , category: 0
        , symbol: "rect"
        , symbolSize: selectionSize key url $ articleSize n key
        }
      KS.TopicNode _ _ ->
        { id: key
        , name: key
        , category: 1
        , symbol: "diamond"
        , symbolSize: selectionSize key key $ topicSize key
        }
      KS.HashTagNode url _ ->
        { id: key
        , name: url
        , category: 2
        , symbol: "rect"
        , symbolSize: selectionSize key url $ hashtagSize key
        }
      KS.ImageNode url ->
        { id: key
        , name: url
        , category: 3
        , symbol: imageSymbol url key
        , symbolSize: selectionSize key url $ imageSize key
        }
      KS.ExternalKitchenSinkSiteNode url ->
        { id: key
        , name: url
        -- unexpanded: hollow triangle (click to fetch + merge that site's
        -- own graph); expanded: filled circle, its nodes/edges are already
        -- spliced into `graph`.
        , category: 4
        , symbol: if url `elem` expandedExternalSites then "circle" else "triangle"
        , symbolSize: searchSize key url $ externalSiteSize key
        }

    echartEdge (Tuple source target) = {source, target}

    categories =
      [ {name:"articles"}
      , {name:"topics"}
      , {name:"hashtags"}
      , {name:"images"}
      , {name:"external"}
      ]

    options = {
      legend: [
        { data: map _.name categories }
      ],
      series: [
        { name: "dependency graph"
        , type: "graph"
        , layout: "force"
        , data: toArrayOf (_TopicGraph <<< to _.nodes <<< traversed <<< to echartNode) graph
        , links: toArrayOf (_TopicGraph <<< to _.edges <<< traversed <<< to echartEdge) graph
        , categories: categories
        , roam: true
        , draggable: true
        , label: {
            position: "right"
          }
        -- 0.5 is the echarts default.
        , lineStyle: {
            opacity: if searching then 0.1 else 0.5
          }
        , force: {
            repulsion: 100
          }
        }
      ]
    }
  in
  {options,modified:true}
