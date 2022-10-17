module Main where

import Prelude

import Affjax.Web as AX
import Data.Lens (view, preview, toArrayOf, traversed, _Just, to)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Maybe (maybe,Maybe(..),fromMaybe)
import Data.String (trim, split, contains, Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.DOM.ParentNode (QuerySelector(..))

import KitchenSink (fetchPaths)
import KitchenSink.Blog (PathList, TargetSummary, _TargetSummary, _TopicSummary, _PreambleSummary, TargetType(..), _PathList)

fetchPathList :: Aff (Maybe PathList)
fetchPathList = do
  res <- fetchPaths
  case res of
    Left err -> do
      log $ "failed: " <> AX.printError err
      pure Nothing
    Right (Left err) -> do
           log $ "failed: " <> show err
           pure Nothing
    Right (Right val) -> pure $ Just val

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  blogPaths <- H.liftAff fetchPathList
  let routes = toArrayOf (_Just <<< _PathList <<< to _.paths <<< traversed) blogPaths
  elem <- HA.selectElement (QuerySelector "#search-box")
  let tgt = fromMaybe body elem
  runUI component {routes: routes} tgt

data Action
  = SetFilterString String
  | SetVisibility Visibility
  | ToggleItemVisibility TargetType String MouseEvent.MouseEvent

data Visibility = Hidden | Visible

data Query a = Query a

type Input =
  { routes :: Array (Tuple String TargetSummary)
  }

type Output = Unit

type State =
  { routes :: Array (Tuple String TargetSummary)
  , filter :: Maybe String
  , visibility :: Visibility
  , expanded :: Maybe String
  }

matchFilter :: String -> String -> Boolean
matchFilter filter str =
  Array.all matchString
  $ split (Pattern " ")
  $ trim filter
  where
    matchString q = Pattern q `contains` str

matchRoute :: String -> Tuple String TargetSummary -> Boolean
matchRoute filter (Tuple r summary) =
  let f = matchFilter filter 
      inTitle = preview (_TargetSummary <<< to _.textualTitle <<< _Just <<< to f) summary
      inSummary = preview (_TargetSummary <<< to _.textualSummary <<< _Just <<< to f) summary
  in
  Foldable.or
  [ f r
  , fromMaybe false inTitle
  , fromMaybe false inSummary
  ]

component
  :: forall m. MonadAff m
  => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
    }
  where

  initialState input = { routes: input.routes , filter: ".html" , visibility: Hidden , expanded: Nothing }

  render state =
    HH.div_
      [ renderFilter state.filter
      , renderRoutes state state.visibility (Array.filter (matchRoute state.filter) state.routes)
      ]

  renderFilter str =
      HH.input
      [ HP.class_ (HH.ClassName "routes-filter-input")
      , HE.onValueInput (\e -> SetFilterString e)
      , HE.onClick (\e -> SetVisibility Visible)
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
      , HH.ul_ $ map (renderRoute state.expanded) rts
      ]

  renderCollapseListButton =
      HH.button
      [ HP.class_ (HH.ClassName "collapse-button")
      , HE.onClick (\e -> SetVisibility Hidden)
      ]
      [ HH.text "🔺️"
      ]

  renderRoute expanded (Tuple r summary) =
    let
      visibilityClass = if Just r == expanded then "visible" else "hidden"
      targetType = view (_TargetSummary <<< to _.targetType) summary
      defaultTitle = showTargetType targetType
      summaryTitle = view (_TargetSummary <<< to _.textualTitle) summary
      summaryDescription = view (_TargetSummary <<< to _.textualSummary) summary
      preambleFavicon = preview (_TargetSummary <<< to _.preambleSummary <<< _Just <<< _PreambleSummary <<< to _.faviconUrl) summary
      topicImage = preview (_TargetSummary <<< to _.topicSummary <<< _Just <<< _TopicSummary <<< to _.imageLink <<< _Just) summary
    in
    HH.li
    [ HP.classes [ HH.ClassName "routes-list-item", HH.ClassName "link-description" , HH.ClassName visibilityClass ] -- note we hide only on div.hidden
    , HE.onClick (\e -> ToggleItemVisibility targetType r e)
    ]
    [ HH.p
      [ HP.class_ (HH.ClassName "link-description-header")
      ]
      [ HH.a
        [ HP.href r
        , HP.target "_blank"
        , HP.class_ (HH.ClassName "link")
        ]
        [ maybe (HH.text "") renderFavicon preambleFavicon
        , HH.span_ [ HH.text r ]
        ]
      , HH.span
        [ HP.class_ (HH.ClassName "link-description-title")
        ]
        [ HH.text $ fromMaybe defaultTitle summaryTitle
        ]
      ]
    , HH.div
      [ HP.classes [ HH.ClassName "link-description-body" , HH.ClassName visibilityClass ]
      ]
      [ HH.p_
        [ HH.text $ fromMaybe "" summaryDescription
        ]
      , maybe (HH.text "") renderTopicImage topicImage
      ]
    ]


  renderFavicon url =
    HH.img
    [ HP.src url
    , HP.width 32
    , HP.height 32
    ]

  renderTopicImage url =
    HH.img
    [ HP.src url
    ]


  handleAction = case _ of
    SetFilterString str -> H.modify_ _ { filter = str } 
    SetVisibility v -> H.modify_ _ { visibility = v }
    ToggleItemVisibility targetType r ev -> do
       st0 <- H.get
       H.put $ st0 { expanded = Just r }
       when (st0.expanded /= Just r && preventMouseClickBeforeExpand targetType) $
         H.liftEffect $ preventDefault (MouseEvent.toEvent ev)

preventMouseClickBeforeExpand :: TargetType -> Boolean
preventMouseClickBeforeExpand = case _ of
  ArticleTarget -> true
  _ -> false

showTargetType :: TargetType -> String
showTargetType = case _ of
  CssTarget -> "A css file"
  ImageTarget -> "Some image"
  GraphVizImageTarget -> "Some rendered graph image"
  VideoTarget -> "Some video"
  JavaScriptSourceTarget -> "Some JS code"
  HtmlSourceTarget -> "Some raw HTML file."
  JSONTarget -> "Some JSON data"
  RawTarget -> "Some arbitrary data"
  RootFileTarget -> "Some unspecified data."
  ArticleTarget -> "An article."
  GeneratedTarget -> "The output of a command."
  TopicsIndexTarget -> "An index."

