module Main where

import Prelude

import Affjax.Web as AX
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Data.Lens
import Data.Tuple (Tuple(..))
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
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (Window, open)

import EChart as EChart
import KSGraph as KSGraph
import KitchenSink (fetchGraph)
import KitchenSink.Blog.Advanced (TopicGraph, _TopicGraph)
import KitchenSink.Blog.Advanced as KS

type BaseUrl = String

getGraph :: BaseUrl -> Aff (Maybe TopicGraph)
getGraph baseUrl = do
  res <- fetchGraph baseUrl
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
  graph <- H.liftAff $ getGraph ""
  elem <- HA.selectElement (QuerySelector "#echartzone")
  let tgt = fromMaybe body elem
  runUI component graph tgt

type Slots = ( ksgraph :: forall query. H.Slot query (EChart.Output KSGraph.Event) Unit  )
_ksgraph = Proxy :: Proxy "ksgraph"

type Input = Maybe TopicGraph

data Action
  = HandleGraphEvent (EChart.Output KSGraph.Event)

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

  initialState graph = {graph, focusedNode: Nothing}

  render state =
    HH.div
    [ HP.class_ $ HH.ClassName "kitchensink-topicgraph"
    ]
    [ case state.graph of
        Nothing -> renderEmpty
        Just graph -> renderGraph graph state.focusedNode
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

  renderGraph graph focusedNode =
    HH.div_
    [ HH.slot _ksgraph unit EChart.component (KSGraph.chartOptions graph focusedNode) HandleGraphEvent
    ]

  handleAction = case _ of
    HandleGraphEvent ev -> do
      let event = KSGraph.runExcept (KSGraph.decodeEvent ev) 
      traverse_ onClick event

  onClick (KSGraph.ClickedNode node) = onNodeClicked node
  onClick _ = pure unit

  onNodeClicked node = do
    st0 <- H.get
    let u = url node =<< st0.graph
    when (map _.id st0.focusedNode == Just node.id) $ do
      H.liftEffect $ traverse_ openPage u
    H.modify_ _ { focusedNode = Just node }

openPage :: String -> Effect (Maybe Window)
openPage url = window >>= open url "_blank" ""

url :: KSGraph.Node -> TopicGraph -> Maybe String
url node graph =
  let
    match (Tuple k _) = k == node.id
    f (Tuple key n) = case n of
      KS.ArticleNode url _ -> Just url
      KS.TopicNode url _ -> Just url
      KS.ImageNode url -> Just url
      KS.ExternalKitchenSinkSiteNode url -> Just url
  in preview (_TopicGraph <<< to _.nodes <<< folded <<< filtered match <<< to f <<< _Just) graph
