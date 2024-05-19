module Main where

import Prelude (Unit, map, bind, discard, pure, show, ($), (<<<), (<>))

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
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

import KitchenSink (fetchPaths)
import KitchenSink.Layout.Blog.Summary (PathList, TargetSummary, _TargetSummary, _TopicSummary, _PreambleSummary, _HashTagSummary, _HashTagItem, TargetType(..), _PathList)

import Searchbox

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
  let props = {matchRoute, renderRoute}
  runUI (component props) {routes: routes, filter: ".html"} tgt

matchFilter :: FilterQuery -> Route -> Boolean
matchFilter filter str =
  Array.all matchString
  $ split (Pattern " ")
  $ trim filter
  where
    matchString q = Pattern q `contains` str

matchRoute :: FilterQuery -> Tuple Route TargetSummary -> Boolean
matchRoute filter (Tuple r summary) =
  let f = matchFilter filter 
      inTitle = preview (_TargetSummary <<< to _.textualTitle <<< _Just <<< to f) summary
      inSummary = preview (_TargetSummary <<< to _.textualSummary <<< _Just <<< to f) summary
      hashtags = toArrayOf (_TargetSummary <<< to _.hashtagSummary <<< _HashTagSummary <<< to _.hashtags <<< traversed <<< _HashTagItem <<< to _.hashtag ) summary
      inHashtags = Foldable.or $ map f $ map ("#" <> _) hashtags
  in
  Foldable.or
  [ f r
  , fromMaybe false inTitle
  , fromMaybe false inSummary
  , inHashtags
  ]

renderRoute :: forall m. ItemVisibility -> Tuple Route TargetSummary -> HH.ComponentHTML Action () m
renderRoute visibility (Tuple r summary) =
  let
    visibilityClass = case visibility of
                        Expanded -> "visible"
                        Collapsed -> "hidden"
    targetType = view (_TargetSummary <<< to _.targetType) summary
    defaultTitle = showTargetType targetType
    summaryTitle = view (_TargetSummary <<< to _.textualTitle) summary
    summaryDescription = view (_TargetSummary <<< to _.textualSummary) summary
    preambleFavicon = preview (_TargetSummary <<< to _.preambleSummary <<< _Just <<< _PreambleSummary <<< to _.faviconUrl) summary
    topicImage = preview (_TargetSummary <<< to _.topicSummary <<< _Just <<< _TopicSummary <<< to _.imageLink <<< _Just) summary
    hashtags = toArrayOf (_TargetSummary <<< to _.hashtagSummary <<< _HashTagSummary <<< to _.hashtags <<< traversed <<< _HashTagItem <<< to _.hashtag ) summary
  in
  HH.li
  [ HP.classes [ HH.ClassName "routes-list-item", HH.ClassName "link-description" , HH.ClassName visibilityClass ] -- note we hide only on div.hidden
  , HE.onClick (\e -> ToggleItemVisibility (preventMouseClickBeforeExpand targetType) r e)
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
    , renderTopicHashtags hashtags
    ]
  , HH.div
    [ HP.classes [ HH.ClassName "link-description-body" , HH.ClassName visibilityClass ]
    ]
    [ HH.p_
      [ HH.text $ fromMaybe "" summaryDescription
      ]
    , maybe (HH.text "") renderTopicImage topicImage
    , fromMaybe (HH.text "") (renderMediaEmbed r targetType)
    ]
  ]


renderFavicon :: forall w i. String -> HH.HTML w i
renderFavicon url =
  HH.img
  [ HP.src url
  , HP.width 32
  , HP.height 32
  ]

renderTopicHashtags :: forall w i. Array String -> HH.HTML w i
renderTopicHashtags tags =
  HH.ul
  [ HP.class_ $ HH.ClassName "hashtags-list"
  ]
  $ map ht tags
  where
    ht t = HH.li [ HP.class_ $ HH.ClassName "hashtags-list-item" ] [ HH.text $ "#" <> t ]

renderTopicImage :: forall w i. String -> HH.HTML w i
renderTopicImage url =
  HH.img
  [ HP.src url
  ]

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
  AudioTarget -> "Some audio file"
  JavaScriptSourceTarget -> "Some JS code"
  HtmlSourceTarget -> "Some raw HTML file"
  JSONTarget -> "Some JSON data"
  RawTarget -> "Some arbitrary data"
  DatasetTarget -> "An article dataset"
  RootFileTarget -> "Some unspecified data"
  ArticleTarget -> "An article"
  GeneratedTarget -> "The output of a command"
  TopicsIndexTarget -> "An index"
  GlossaryTarget -> "The glossary"
  DocumentTarget -> "Some document file"
  HashTagsIndexTarget -> "Hashtags index"
  WebfontTarget -> "Some web font"

renderMediaEmbed :: forall w i. Route -> TargetType -> Maybe (HH.HTML w i)
renderMediaEmbed r =
  case _ of
    ImageTarget -> Just $
      renderTopicImage r
    GraphVizImageTarget -> Just $
      renderTopicImage r
    VideoTarget -> Just $
      HH.video
      [ HP.controls true
      , HP.width 320
      , HP.height 240
      ]
      [ HH.source
        [ HP.src r
        ]
      ]
    AudioTarget -> Just $
      HH.audio
      [ HP.controls true
      ]
      [ HH.source
        [ HP.src r
        ]
      ]
    _ -> Nothing
