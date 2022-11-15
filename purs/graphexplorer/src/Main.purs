module Main where

import Prelude

import Affjax.Web as AX
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import KitchenSink (fetchGraph)
import KitchenSink.Blog.Advanced (TopicGraph)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

import EChart as EChart
import KSGraph as KSGraph

getGraph :: Aff (Maybe TopicGraph)
getGraph = do
  res <- fetchGraph
  case res of
    Left err -> do 
      liftEffect $ log $ "failed: " <> AX.printError err
      pure Nothing
    Right (Left err) -> do
           liftEffect $ log $ "failed: " <> show err
           pure Nothing
    Right (Right val) -> pure $ Just val

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  graph <- H.liftAff getGraph
  elem <- HA.selectElement (QuerySelector "#graph-explorer")
  let tgt = fromMaybe body elem
  runUI component graph tgt

type Slots = ( ksgraph :: forall query. H.Slot query (EChart.Output KSGraph.Event) Unit  )
_ksgraph = Proxy :: Proxy "ksgraph"

type Input = Maybe TopicGraph

data Action
  = Noop

component
  :: forall query output m. MonadAff m
  => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
    }
  where

  initialState graph = {graph}

  render state =
    HH.div
    [ HP.class_ $ HH.ClassName "kitchensink-topicgraph"
    ]
    [ case state.graph of
        Nothing -> renderEmpty
        Just graph -> renderGraph graph
    ]

  renderEmpty =
    HH.div
    [ HP.class_ $ HH.ClassName "errorbox"
    ]
    [ HH.p
      [ HP.class_ $ HH.ClassName "errorbox-message"
      ]
      [ HH.text "could not load the topics graph"
      ]
    ]

  renderGraph graph =
    HH.div_
    [ HH.text "got a graph"
    , HH.slot_ _ksgraph unit EChart.component (KSGraph.chartOptions graph)
    ]

  handleAction = case _ of
    Noop -> pure unit
