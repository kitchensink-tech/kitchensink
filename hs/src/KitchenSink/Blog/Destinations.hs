
module KitchenSink.Blog.Destinations (Tag, destTag, OutputPrefix) where

import Data.Text (Text)
import qualified Data.Text as Text

import KitchenSink.Blog.Target
import KitchenSink.Blog.Prelude
import System.FilePath.Posix ((</>))

type OutputPrefix = FilePath

type Tag = Text

destTag :: OutputPrefix -> Tag -> DestinationLocation
destTag prefix tag = VirtualFileDestination
    (Text.pack $ "/topics/" <> tagFileName tag)
    (prefix </> "topics" </> tagFileName tag)

tagFileName :: Tag -> FilePath
tagFileName t = Text.unpack (Text.replace " " "-" t) <> ".html"

