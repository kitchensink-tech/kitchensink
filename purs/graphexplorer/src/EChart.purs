
module EChart where

import Effect (Effect)
import Effect.Console (log)
import Prelude
import Data.Traversable
import Data.Maybe

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Subscription (Listener, create, notify)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Echarts as Echarts

type Options o = {|o}

type Output i = {|i}

data Action o i
  = Initialize
  | Render
  | Update (Options o)
  | OnEvent (Output i)

type State o =
  { options :: (Options o)
  , chart   :: Maybe Echarts.ChartRef
  }

type Input o =
  { options  :: Options o
  , modified :: Boolean -- allows to pick whether to auto-update on any refresh or use Query-based
  }

data Query o a =
    SetOptions (Options o) a
  | ReRender a

component
  :: forall option item m. MonadAff m
  => H.Component (Query option) (Input option) (Output item) m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = \input -> if input.modified then Just (Update input.options) else Nothing
      , handleQuery = handleQuery
      }
    }
  where
    initialState i = { options: i.options, chart: Nothing }

    render _ =
      HH.div
      [ HP.class_ $ HH.ClassName "echarts-container"
      ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "echarts-ref"
        , HP.style "width:600px;height:400px;"
        , HP.ref (H.RefLabel "chart-ref")
        ]
        [
        ]
      ]

    handleQuery :: forall a. Query option a -> H.HalogenM (State option) (Action option item) () (Output item) m (Maybe a)
    handleQuery  = case _ of
      SetOptions options a -> do
        handleAction $ Update options
        pure (Just a)
      ReRender a -> do
        handleAction $ Render
        pure (Just a)
  
    handleAction = case _ of
      Initialize -> do
        H.getRef (H.RefLabel "chart-ref") >>= traverse_ \element -> do
          obj <- H.liftEffect $ do
            { emitter, listener } <- create
            chart <- Echarts.init element
            Echarts.showLoading chart
            Echarts.on chart "click" (\cbdata -> notify listener (OnEvent cbdata))
            pure {chart, emitter}
          _ <- H.subscribe obj.emitter
          st1 <- H.modify _ { chart = Just obj.chart }
          handleAction $ Render
      Update options -> do
        H.modify_ _ { options = options }
        handleAction $ Render
      Render -> do
        st0 <- H.get
        case st0.chart of
          Nothing -> pure unit
          Just chart -> do
            H.liftEffect $ do
              Echarts.hideLoading chart
              Echarts.setOptions chart st0.options
      OnEvent item -> do
        H.raise item
