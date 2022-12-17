
module KitchenSink.Layout.Blog.Destinations
where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath.Posix ((</>), takeFileName, takeBaseName)

import KitchenSink.Prelude
import KitchenSink.Core.Section.Base as Core
import KitchenSink.Core.Section (GeneratorInstructionsData(..))
import KitchenSink.Core.Build.Target (OutputPrefix, SourceLocation(..),DestinationLocation(..))

type Tag = Text

tagFileName :: Tag -> FilePath
tagFileName t = Text.unpack (Text.replace " " "-" t) <> ".html"

tagAtomName :: Tag -> FilePath
tagAtomName t = Text.unpack (Text.replace " " "-" t) <> ".atom"

data GenFileExtension
  = GenPngFile

extensionString :: GenFileExtension -> String
extensionString GenPngFile = ".png"

destTopic :: OutputPrefix -> Tag -> DestinationLocation
destTopic prefix tag = VirtualFileDestination
    (Text.pack $ "/topics/" <> tagFileName tag)
    (prefix </> "topics" </> tagFileName tag)

destTopicAtom :: OutputPrefix -> Tag -> DestinationLocation
destTopicAtom prefix tag = VirtualFileDestination
    (Text.pack $ "/topics/" <> tagAtomName tag)
    (prefix </> "topics" </> tagAtomName tag)

destGenImage :: OutputPrefix -> SourceLocation -> GenFileExtension -> DestinationLocation
destGenImage prefix (FileSource path) ext =
  StaticFileDestination
    (Text.pack $ "/gen/images/" <> takeFileName path <> extensionString ext)
    (prefix </> "gen/images" </> takeFileName path <> extensionString ext)

destGenArbitrary :: OutputPrefix -> SourceLocation -> GeneratorInstructionsData -> DestinationLocation
destGenArbitrary prefix (FileSource path) g =
  StaticFileDestination
    (Text.pack $ "/gen/out/" <> takeFileName path <> "__" <> target g)
    (prefix </> "gen/out" </> takeFileName path <> "__" <> target g)

newtype FileExtension = FileExtension String

destinationExtension :: Format -> FileExtension
destinationExtension fmt = FileExtension $ case fmt of
  Core.Json -> "json"
  Core.Cmark -> "cmark"
  Core.Dhall -> "dhall"
  Core.TextHtml -> "html"
  Core.Css -> "css"
  Core.Csv -> "csv"
  Core.InMemory -> "mem"

destEmbeddedData :: OutputPrefix -> SourceLocation -> FileExtension -> Int -> DestinationLocation
destEmbeddedData prefix (FileSource path) (FileExtension ext) index =
  StaticFileDestination
    (Text.pack $ "/raw/data/" <> takeFileName path <> "__" <> show index <> "." <> ext)
    (prefix </> "raw/data" </> takeFileName path <> "__" <> show index <> "." <> ext)

destVideoFile :: OutputPrefix -> SourceLocation -> DestinationLocation
destVideoFile prefix (FileSource path) =
  StaticFileDestination
    (Text.pack $ "/videos/" <> takeFileName path)
    (prefix </> "videos" </> takeFileName path)

destRawFile :: OutputPrefix -> SourceLocation -> DestinationLocation
destRawFile prefix (FileSource path)
  | takeFileName path == "robots.txt" =
      StaticFileDestination
        (Text.pack $ "/robots.txt")
        (prefix </> "robots.txt")
  | otherwise =
      StaticFileDestination
        (Text.pack $ "/raw/" <> takeFileName path)
        (prefix </> "raw" </> takeFileName path)

destImage :: OutputPrefix -> SourceLocation -> DestinationLocation
destImage prefix (FileSource path) =
  StaticFileDestination
    (Text.pack $ "/images/" <> takeFileName path)
    (prefix </> "images" </> takeFileName path)

destCssFile :: OutputPrefix -> SourceLocation -> DestinationLocation
destCssFile prefix (FileSource path) =
  StaticFileDestination
    (Text.pack $ "/css/" <> takeFileName path)
    (prefix </> "css" </> takeFileName path)

destJsFile :: OutputPrefix -> SourceLocation -> DestinationLocation
destJsFile prefix (FileSource path) =
  StaticFileDestination
    (Text.pack $ "/js/" <> takeFileName path)
    (prefix </> "js" </> takeFileName path)

destJsonDataFile :: OutputPrefix -> FilePath -> DestinationLocation
destJsonDataFile prefix path =
  StaticFileDestination
    (Text.pack $ "/json/" <> takeFileName path)
    (prefix </> "json" </> takeFileName path)

destRootDataFile :: OutputPrefix -> FilePath -> DestinationLocation
destRootDataFile prefix path
  | otherwise =
      StaticFileDestination
        (Text.pack $ "/" <> takeFileName path)
        (prefix </> takeFileName path)

destHtml :: OutputPrefix -> SourceLocation -> DestinationLocation
destHtml prefix (FileSource path) =
  StaticFileDestination
    (Text.pack $ "/" <> takeBaseName path <> ".html")
    (prefix </> takeBaseName path <> ".html")
