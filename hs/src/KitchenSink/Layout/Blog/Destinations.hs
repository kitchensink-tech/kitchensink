
module KitchenSink.Layout.Blog.Destinations
where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath.Posix ((</>), takeFileName, takeBaseName)

import KitchenSink.Prelude
import KitchenSink.Core.Section (GeneratorInstructionsData(..))
import KitchenSink.Core.Build.Target (SourceLocation(..),DestinationLocation(..))

type OutputPrefix = FilePath

type Tag = Text

tagFileName :: Tag -> FilePath
tagFileName t = Text.unpack (Text.replace " " "-" t) <> ".html"

data GenFileExtension
  = GenPngFile

extensionString :: GenFileExtension -> String
extensionString GenPngFile = ".png"

destTag :: OutputPrefix -> Tag -> DestinationLocation
destTag prefix tag = VirtualFileDestination
    (Text.pack $ "/topics/" <> tagFileName tag)
    (prefix </> "topics" </> tagFileName tag)

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
