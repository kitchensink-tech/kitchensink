module Main where

import Prelude

import Affjax.Web as AX
import Data.Array (elem, (\\), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Data.Lens
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
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
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (Window, open)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import ChartResize (resizeChartsIn)

import Halogen.ECharts as ECharts
import KSGraph as KSGraph
import KitchenSink (fetchGraph, getBasePath)
import KitchenSink.Layout.Blog.Analyses.SiteGraph (TopicGraph(..), _TopicGraph)
import KitchenSink.Layout.Blog.Analyses.SiteGraph as KS

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
  basePath <- liftEffect $ getBasePath "echartzone"
  graph <- H.liftAff $ getGraph basePath
  elem <- HA.selectElement (QuerySelector "#echartzone")
  let tgt = fromMaybe body elem
  runUI component graph tgt

type Slots = ( ksgraph :: forall query. H.Slot query (ECharts.Output KSGraph.Event) Unit  )
_ksgraph = Proxy :: Proxy "ksgraph"

type Input = Maybe TopicGraph

data Action
  = Initialize
  | HandleGraphEvent (ECharts.Output KSGraph.Event)
  | ToggleEnlarged
  | LeaveEnlarged
  | ViewportResized

-- | The chart zone reads its size from two CSS custom properties, so the
-- same `Halogen.ECharts` component (whose style string is fixed once at
-- creation) can be switched between the inline size (the fallbacks below)
-- and the enlarged view (the properties set by `enlargedStyle`).
chartStyle :: String
chartStyle =
  "width:var(--kitchensink-topicgraph-width,640px);"
    <> "height:var(--kitchensink-topicgraph-height,480px);"

-- | Inline view: the widget is only a positioning context for the toggle.
inlineStyle :: String
inlineStyle = "position:relative;width:640px;max-width:100%;"

-- | Enlarged view: the widget covers the whole viewport. `--bg` is the
-- page background of the stock stylesheets (`colors.css`); `Canvas` is the
-- fallback for sites that do not define it.
enlargedStyle :: String
enlargedStyle =
  "position:fixed;top:0;right:0;bottom:0;left:0;z-index:10000;"
    <> "background:var(--bg,Canvas);"
    <> "--kitchensink-topicgraph-width:100%;"
    <> "--kitchensink-topicgraph-height:100vh;"

toggleStyle :: String
toggleStyle = "position:absolute;top:0.5em;right:0.5em;z-index:1;cursor:pointer;"

-- | The DOM elements the ECharts instances of this widget are mounted on.
chartSelector :: String
chartSelector = ".kitchensink-topicgraph .echarts-ref"

component
  :: forall query output m. MonadAff m
  => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where

  initialState graph = {graph, focusedNode: Nothing, expandedSites: [], enlarged: false}

  render state =
    HH.div
    [ HP.classes $ map HH.ClassName $
        if state.enlarged
          then ["kitchensink-topicgraph", "kitchensink-topicgraph-enlarged"]
          else ["kitchensink-topicgraph"]
    , HP.style $ if state.enlarged then enlargedStyle else inlineStyle
    ]
    case state.graph of
      Nothing -> [ renderEmpty ]
      Just graph ->
        [ renderToggle state.enlarged
        , renderGraph graph state.focusedNode state.expandedSites
        ]

  renderToggle enlarged =
    HH.button
    [ HP.class_ $ HH.ClassName "kitchensink-topicgraph-toggle"
    , HP.type_ HP.ButtonButton
    , HP.style toggleStyle
    , HP.title $ if enlarged then "back to the inline view (Esc)" else "enlarge the graph to the whole window"
    , HPA.pressed $ if enlarged then "true" else "false"
    , HE.onClick \_ -> ToggleEnlarged
    ]
    [ HH.text $ if enlarged then "close" else "enlarge"
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

  renderGraph graph focusedNode expandedSites =
    HH.div_
    [ HH.slot _ksgraph unit (ECharts.component chartStyle) (KSGraph.chartOptions graph focusedNode expandedSites) HandleGraphEvent
    ]

  handleAction = case _ of
    Initialize -> do
      win <- H.liftEffect window
      doc <- H.liftEffect $ Window.document win
      void $ H.subscribe $ eventListener KET.keydown (HTMLDocument.toEventTarget doc)
        \ev -> case map KE.key (KE.fromEvent ev) of
          Just "Escape" -> Just LeaveEnlarged
          _ -> Nothing
      void $ H.subscribe $ eventListener (EventType "resize") (Window.toEventTarget win)
        \_ -> Just ViewportResized
    HandleGraphEvent ev -> do
      let event = KSGraph.runExcept (KSGraph.decodeEvent ev)
      traverse_ onClick event
    ToggleEnlarged -> do
      st0 <- H.get
      setEnlarged (not st0.enlarged)
    LeaveEnlarged -> do
      st0 <- H.get
      when st0.enlarged $ setEnlarged false
    ViewportResized ->
      H.liftEffect $ resizeChartsIn chartSelector

  -- The state change re-renders the widget (new size on the chart zone)
  -- before the bind continues, so the chart re-measures the resized zone.
  setEnlarged enlarged = do
    H.modify_ _ { enlarged = enlarged }
    H.liftEffect $ resizeChartsIn chartSelector

  onClick (KSGraph.ClickedNode node) = onNodeClicked node
  onClick _ = pure unit

  onNodeClicked node
    | node.category == KSGraph.ExternalSites = onExternalSiteNodeClicked node
    | otherwise = onOrdinaryNodeClicked node

  onOrdinaryNodeClicked node = do
    st0 <- H.get
    let u = url node =<< st0.graph
    when (map _.id st0.focusedNode == Just node.id) $ do
      H.liftEffect $ traverse_ openPage u
    H.modify_ _ { focusedNode = Just node }

  -- `node.name` is the external site's own URL for an
  -- `ExternalKitchenSinkSiteNode` (see `KSGraph.echartNode`).
  --
  -- Click-to-expand, not eager-fetch-on-load: eagerly fetching every
  -- external site referenced by a site (possibly transitively, once
  -- merged) doesn't scale and wasn't asked for, so a first click on an
  -- unexpanded external-site node fetches that site's `topicsgraph.json`
  -- and splices it into the local graph instead of just focusing it.
  --
  -- Cycle guard: `expandedSites` accumulates the URLs of sites already
  -- merged in. If a merged remote graph itself references a site we've
  -- already expanded (A -> B -> A, or a direct self-reference), the
  -- resulting node is rendered normally, but clicking it re-enters this
  -- same guard and is a no-op fetch (falls into the "already expanded"
  -- branch below) rather than looping.
  onExternalSiteNodeClicked node = do
    st0 <- H.get
    let siteUrl = node.name
    if siteUrl `elem` st0.expandedSites
      then do
        -- Already expanded: behave like an ordinary node (focus, then
        -- open the site itself in a new tab on a second click).
        when (map _.id st0.focusedNode == Just node.id) $
          H.liftEffect $ traverse_ openPage (Just siteUrl)
        H.modify_ _ { focusedNode = Just node }
      else do
        H.modify_ _ { focusedNode = Just node }
        mMerged <- H.liftAff $ fetchAndMergeExternalGraph siteUrl st0.graph
        case mMerged of
          Nothing -> pure unit
          Just merged ->
            H.modify_ _
              { graph = Just merged
              , expandedSites = siteUrl : st0.expandedSites
              }

-- | Fetches `<siteUrl>/json/topicsgraph.json` (the same shape kitchen-sink
-- emits for its own site, see `KitchenSink.fetchGraph`) and merges it into
-- the local graph. `Nothing` on any failure (network error, decode error,
-- or no local graph loaded yet) — the caller leaves the graph untouched in
-- that case.
fetchAndMergeExternalGraph :: String -> Maybe TopicGraph -> Aff (Maybe TopicGraph)
fetchAndMergeExternalGraph siteUrl mLocal = do
  mRemote <- getGraph (stripTrailingSlash siteUrl)
  pure $ case mLocal, mRemote of
    Just local, Just remote -> Just (mergeExternalGraph siteUrl local remote)
    _, _ -> Nothing

-- | Splices a remote site's topic graph into the local one:
--
--   * every remote node/edge key is namespaced with the site's URL, so it
--     cannot collide with a local key (or with another already-merged
--     site's keys);
--   * the local `ExternalKitchenSinkSiteNode` that was clicked gets extra
--     edges to the remote graph's own "roots" (nodes that are nobody's
--     edge target in the remote graph, typically its topics), so the
--     merged graph reads as one connected component instead of a
--     disconnected island next to the site node that was expanded.
mergeExternalGraph :: String -> TopicGraph -> TopicGraph -> TopicGraph
mergeExternalGraph siteUrl (TopicGraph local) (TopicGraph remote) =
  let
    ns k = siteUrl <> "::" <> k

    remoteNodes = map (\(Tuple k n) -> Tuple (ns k) (absolutizeNode siteUrl n)) remote.nodes
    remoteEdges = map (\(Tuple a b) -> Tuple (ns a) (ns b)) remote.edges

    remoteKeys = map fst remoteNodes
    remoteTargets = map snd remoteEdges
    roots = remoteKeys \\ remoteTargets

    localSiteKey = "site:" <> siteUrl
    connectingEdges = map (\r -> Tuple localSiteKey r) roots
  in
    TopicGraph
      { nodes: local.nodes <> remoteNodes
      , edges: local.edges <> remoteEdges <> connectingEdges
      }

stripTrailingSlash :: String -> String
stripTrailingSlash u = fromMaybe u (String.stripSuffix (Pattern "/") u)

-- | `"https://host/sub/"` -> `"https://host"`; `""` when `siteUrl` carries no
-- scheme (nothing sensible to resolve against).
siteOrigin :: String -> String
siteOrigin siteUrl = case String.indexOf (Pattern "://") siteUrl of
  Nothing -> ""
  Just i ->
    let hostStart = i + 3
    in case String.indexOf (Pattern "/") (String.drop hostStart siteUrl) of
      Nothing -> siteUrl
      Just j -> String.take (hostStart + j) siteUrl

-- | A remote site's graph carries root-relative URLs (`/sub/page.html`,
-- already including that site's own basePath). Left as is, they would
-- resolve against the site *displaying* the graph, so they are made
-- absolute against the remote site's origin when merged.
absolutizeNode :: String -> KS.Node -> KS.Node
absolutizeNode siteUrl = case _ of
  KS.ArticleNode u n -> KS.ArticleNode (abs u) n
  KS.TopicNode u n -> KS.TopicNode (abs u) n
  KS.HashTagNode u n -> KS.HashTagNode (abs u) n
  KS.ImageNode u -> KS.ImageNode (abs u)
  KS.ExternalKitchenSinkSiteNode u -> KS.ExternalKitchenSinkSiteNode u
  where
  abs u
    | String.take 1 u == "/" && String.take 2 u /= "//" = siteOrigin siteUrl <> u
    | otherwise = u

openPage :: String -> Effect (Maybe Window)
openPage url = window >>= open url "_blank" ""

url :: KSGraph.Node -> TopicGraph -> Maybe String
url node graph =
  let
    match (Tuple k _) = k == node.id
    f (Tuple key n) = case n of
      KS.ArticleNode url _ -> Just url
      KS.TopicNode url _ -> Just url
      KS.HashTagNode url _ -> Just url
      KS.ImageNode url -> Just url
      KS.ExternalKitchenSinkSiteNode url -> Just url
  in preview (_TopicGraph <<< to _.nodes <<< folded <<< filtered match <<< to f <<< _Just) graph
