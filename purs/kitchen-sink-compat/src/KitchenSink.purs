module KitchenSink where

import Prelude (bind, pure, ($), (<>))

import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Either (Either)
import Data.Functor (map)
import Effect.Aff (Aff)
import Affjax.Web as Web
import Affjax.ResponseFormat (json)

import KitchenSink.Layout.Blog.Analyses.SiteGraph (TopicGraph)
import KitchenSink.Layout.Blog.Summary (PathList)

topicsGraphPath :: String
topicsGraphPath = "/json/topicsgraph.json"

sitePaths :: String
sitePaths = "/json/paths.json"

type BaseUrl = String

fetchGraph :: BaseUrl -> Aff (Either Web.Error (Either JsonDecodeError TopicGraph))
fetchGraph baseUrl = do
  resp <- Web.get json (baseUrl <> topicsGraphPath)
  pure $ map (\x -> genericDecodeAeson defaultOptions x.body) resp

fetchPaths :: Aff (Either Web.Error (Either JsonDecodeError PathList))
fetchPaths = do
  resp <- Web.get json sitePaths
  pure $ map (\x -> genericDecodeAeson defaultOptions x.body) resp
