
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
import Data.Maybe (Maybe(..), maybe)
import Data.Array (concat)
import Data.Lens (view, toArrayOf, traversed, to, filtered)
import EChart as EChart

import KitchenSink.Blog.Advanced (TopicGraph, _TopicGraph)
import KitchenSink.Blog.Advanced as KS

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
  | Images

readCategory :: Int -> Except (NonEmptyList ForeignError) Category
readCategory = case _ of
  0 -> pure Articles
  1 -> pure Topics
  2 -> pure Images
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
  , data :: Array { id :: String, name :: String, category :: Int , symbol :: String }
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

chartOptions :: TopicGraph -> Maybe Node -> EChart.Input Options
chartOptions graph focusedNode =
  let
    
    imageSymbol :: String -> String -> String
    imageSymbol url n1id
      | maybe false (\n2 -> n2.id == n1id) focusedNode = "image://" <> url
      | otherwise = "circle"

    echartNode (Tuple key node) = case node of
      KS.ArticleNode title _ -> {id: key, name:title, category: 0, symbol: "rect"}
      KS.TopicNode _ -> {id: key, name:key, category: 1, symbol: "diamond"}
      KS.ImageNode url -> {id: key, name:url, category: 2, symbol: (imageSymbol url key)}

    echartEdge (Tuple source target) = {source, target}

    categories = [{name:"articles"}, {name:"topics"}, {name:"images"}]

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
