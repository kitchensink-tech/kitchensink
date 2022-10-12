module KitchenSink where

import Prelude (bind, pure, ($))

import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Either (Either)
import Data.Functor (map)
import Effect.Aff (Aff)
import Affjax.Web as Web
import Affjax.ResponseFormat (json)

import KitchenSink.Blog.Advanced (PathList, TopicGraph)
-- import Bridge

topicsGraphPath :: String
topicsGraphPath = "/json/topicsgraph.json"

sitePaths :: String
sitePaths = "/json/paths.json"

fetchGraph :: Aff (Either Web.Error (Either JsonDecodeError TopicGraph))
fetchGraph = do
  resp <- Web.get json topicsGraphPath
  pure $ map (\x -> genericDecodeAeson defaultOptions x.body) resp

fetchPaths :: Aff (Either Web.Error (Either JsonDecodeError PathList))
fetchPaths = do
  resp <- Web.get json sitePaths
  pure $ map (\x -> genericDecodeAeson defaultOptions x.body) resp
