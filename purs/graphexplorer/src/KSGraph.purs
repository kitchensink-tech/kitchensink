
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
import Data.Array (concat)
import Data.Lens (view, toArrayOf, traversed, to, filtered)
import Data.Lens.Fold (anyOf)
import Data.Number as Number

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

readCategory :: Int -> Except (NonEmptyList ForeignError) Category
readCategory = case _ of
  0 -> pure Articles
  1 -> pure Topics
  3 -> pure HashTags
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
  , data :: Array { id :: String, name :: String, category :: Int , symbol :: String , symbolSize :: Number }
  , links :: Array { source :: String, target:: String }
  , categories :: Array { name::String }
  , roam :: Boolean
  , draggable :: Boolean
  , label :: { position :: String }
  , force :: { repulsion :: Int }
  }

type Options =
  ( legend :: Array Legend
  , series :: Array Series
  )

chartOptions :: TopicGraph -> Maybe Node -> ECharts.Input Options
chartOptions graph focusedNode =
  let
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
      | isSelection n1id || isSelectionNeighbor n1id = "image://" <> url
      | otherwise = "circle"

    selectionSize :: String -> Number -> Number
    selectionSize n1id default
      | isSelection n1id = 35.0
      | isSelectionNeighbor n1id = 30.0
      | otherwise = default

    articleSize :: Int -> String -> Number
    articleSize n _ = 10.0 + Number.log(toNumber n) / Number.log(2.0)

    topicSize :: String -> Number
    topicSize _ = 10.0

    hashtagSize :: String -> Number
    hashtagSize _ = 8.0

    imageSize :: String -> Number
    imageSize _ = 5.0

    externalSiteSize :: String -> Number
    externalSiteSize _ = 15.0

    echartNode (Tuple key node) = case node of
      KS.ArticleNode url n ->
        { id: key
        , name: url
        , category: 0
        , symbol: "rect"
        , symbolSize: selectionSize key $ articleSize n key
        }
      KS.TopicNode _ _ ->
        { id: key
        , name: key
        , category: 1
        , symbol: "diamond"
        , symbolSize: selectionSize key $ topicSize key
        }
      KS.HashTagNode url n ->
        { id: key
        , name: url
        , category: 2
        , symbol: "rect"
        , symbolSize: selectionSize key $ hashtagSize key
        }
      KS.ImageNode url ->
        { id: key
        , name: url
        , category: 3
        , symbol: imageSymbol url key
        , symbolSize: selectionSize key $ imageSize key
        }
      KS.ExternalKitchenSinkSiteNode url ->
        { id: key
        , name: url
        , category: 4
        , symbol: "triangle"
        , symbolSize: externalSiteSize key
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
        , force: {
            repulsion: 100
          }
        }
      ]
    }
  in
  {options,modified:true}
