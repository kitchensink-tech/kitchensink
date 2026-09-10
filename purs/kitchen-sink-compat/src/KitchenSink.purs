module KitchenSink where

import Prelude (bind, pure, ($), (<>))

import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Either (Either)
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Affjax.Web as Web
import Affjax.ResponseFormat (json)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (getAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

import KitchenSink.Layout.Blog.Analyses.SiteGraph (TopicGraph)
import KitchenSink.Layout.Blog.Summary (PathList)

topicsGraphPath :: String
topicsGraphPath = "/json/topicsgraph.json"

sitePaths :: String
sitePaths = "/json/paths.json"

type BaseUrl = String

-- | Reads the `data-base-path` attribute off the element with the given id.
-- Kitchen-sink's server-side rendering sets this to the site's configured
-- `basePath` (e.g. "/tramaj", or "" when hosted at the domain root; see
-- `KitchenSink.Layout.Blog.Fragments.searchBox`/`siteGraphEchartZone` on the
-- Haskell side), so client-side widgets can fetch their JSON data from the
-- right place even when the site is hosted under a subpath.
getBasePath :: String -> Effect BaseUrl
getBasePath elemId = do
  win <- window
  doc <- document win
  mEl <- getElementById elemId (toNonElementParentNode (toDocument doc))
  case mEl of
    Nothing -> pure ""
    Just el -> map (fromMaybe "") (getAttribute "data-base-path" el)

fetchGraph :: BaseUrl -> Aff (Either Web.Error (Either JsonDecodeError TopicGraph))
fetchGraph baseUrl = do
  resp <- Web.get json (baseUrl <> topicsGraphPath)
  pure $ map (\x -> genericDecodeAeson defaultOptions x.body) resp

fetchPaths :: BaseUrl -> Aff (Either Web.Error (Either JsonDecodeError PathList))
fetchPaths baseUrl = do
  resp <- Web.get json (baseUrl <> sitePaths)
  pure $ map (\x -> genericDecodeAeson defaultOptions x.body) resp
