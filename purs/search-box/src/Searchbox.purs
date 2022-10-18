module Searchbox where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as MouseEvent

data Visibility = Hidden | Visible

type PreventMouseClickBubbling = Boolean

data Action
  = SetFilterString String
  | SetVisibility Visibility
  | ToggleItemVisibility PreventMouseClickBubbling Route MouseEvent.MouseEvent

data Query a = Query a

type Route = String

type FilterQuery = String

type Input summary =
  { routes :: Array (Tuple Route summary)
  , filter :: FilterQuery
  }

type Output = Unit

type State summary =
  { routes :: Array (Tuple Route summary)
  , filter :: FilterQuery
  , visibility :: Visibility
  , expanded :: Maybe Route
  }

data ItemVisibility
  = Expanded
  | Collapsed

type Props m summary =
  { matchRoute :: FilterQuery -> Tuple Route summary -> Boolean
  , renderRoute :: ItemVisibility -> Tuple Route summary -> HH.ComponentHTML Action () m
  }

component
  :: forall summary m. MonadAff m
  => Props m summary
  -> H.Component Query (Input summary) Output m
component props =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
    }
  where

  initialState :: Input summary -> State summary
  initialState input = { routes: input.routes , filter: input.filter , visibility: Hidden , expanded: Nothing }

  render state =
    HH.div_
      [ renderFilter state.filter
      , renderRoutes state state.visibility (Array.filter (props.matchRoute state.filter) state.routes)
      ]

  renderFilter str =
      HH.input
      [ HP.class_ (HH.ClassName "routes-filter-input")
      , HE.onValueInput (\e -> SetFilterString e)
      , HE.onClick (\_ -> SetVisibility Visible)
      , HP.value str
      , HP.placeholder "search"
      ]

  renderRoutes state visibility rts =
      let visibilityClass =
             case _ of
               Hidden -> "hidden"
               Visible -> if Array.null rts then "hidden" else "visible"
      in
      HH.div
      [ HP.classes [ HH.ClassName "routes-list", HH.ClassName $ visibilityClass visibility ]
      ]
      [ HH.div_ [ renderCollapseListButton ]
      , HH.ul_ $ map (\x -> props.renderRoute (itemVisibility state.expanded x) x) rts
      ]

  itemVisibility expanded (Tuple r _) =
    if Just r == expanded
    then Expanded
    else Collapsed

  renderCollapseListButton =
      HH.button
      [ HP.class_ (HH.ClassName "collapse-button")
      , HE.onClick (\_ -> SetVisibility Hidden)
      ]
      [ HH.text "🔺️hide results"
      ]

  handleAction = case _ of
    SetFilterString str -> H.modify_ _ { filter = str } 
    SetVisibility v -> H.modify_ _ { visibility = v }
    ToggleItemVisibility preventMouseClickBeforeExpand r ev -> do
       st0 <- H.get
       H.put $ st0 { expanded = Just r }
       when (st0.expanded /= Just r && preventMouseClickBeforeExpand) $
         H.liftEffect $ preventDefault (MouseEvent.toEvent ev)
