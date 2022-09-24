module Main where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..),fromMaybe)
import Data.String (trim, split, contains, Pattern(..))
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
import Web.DOM.ParentNode (QuerySelector(..))


type Routes = Array String

decodeStringArray :: J.Json -> Either CA.JsonDecodeError Routes
decodeStringArray = CA.decode (CA.array CA.string)

fetchRoutes :: Aff (Maybe Routes)
fetchRoutes = do
  res <- AX.get ResponseFormat.json "/json/paths.json" 
  case res of
    Left err -> do
      log $ "failed: " <> AX.printError err
      pure Nothing
    Right rsp -> do
      let routes = decodeStringArray rsp.body
      case routes of 
         Left err -> do
           log $ "failed: " <> show err
           pure Nothing
         Right val -> pure $ Just val

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  routes <- H.liftAff fetchRoutes
  elem <- HA.selectElement (QuerySelector "#search-box")
  let tgt = fromMaybe body elem
  runUI component {routes: fromMaybe [] routes} tgt

data Action
  = SetFilterString String

data Query a = Query a

type Input =
  { routes :: Array String
  }

type Output = Unit

type State =
  { routes :: Array String
  , filter :: Maybe String
  }

matchFilter :: String -> String -> Boolean
matchFilter filter str =
  Array.all matchString
  $ split (Pattern " ")
  $ trim filter
  where
    matchString q = Pattern q `contains` str

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

  initialState input = { routes: input.routes , filter: ".html" }

  render state =
    HH.div_
      [ renderFilter state.filter
      , renderRoutes (Array.filter (matchFilter state.filter) state.routes)
      ]

  renderFilter str =
      HH.input
      [ HP.class_ (HH.ClassName "routes-filter-input")
      , HE.onValueInput (\e -> SetFilterString e)
      , HP.value str
      , HP.placeholder "search"
      ]

  renderRoutes rts =
      HH.ul
      [ HP.class_ (HH.ClassName "routes-list")
      ]
      $ map renderRoute
      $ rts

  renderRoute r =
      HH.li
      [ HP.class_ (HH.ClassName "routes-list-item")
      ]
      [ HH.a
        [ HP.href r
        , HP.target "_blank"
        , HP.class_ (HH.ClassName "link")
        ]
        [ HH.text r
        ]
      ]

  handleAction = case _ of
    SetFilterString str -> H.modify_ _ { filter = str } 
