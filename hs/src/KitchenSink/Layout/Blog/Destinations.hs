module KitchenSink.Layout.Blog.Destinations
where

import Data.Text qualified as Text
import System.FilePath.Posix (takeBaseName, takeFileName, (</>))

import KitchenSink.Core.Build.Target (DestinationLocation (..), OutputPrefix, SourceLocation (..))
import KitchenSink.Core.Section (GeneratorInstructionsData (..))
import KitchenSink.Core.Section.Base as Core
import KitchenSink.Core.Section.Payloads (TopicName)
import KitchenSink.Prelude

topicFileName :: TopicName -> FilePath
topicFileName t = Text.unpack (Text.replace " " "-" t) <> ".html"

topicAtomName :: TopicName -> FilePath
topicAtomName t = Text.unpack (Text.replace " " "-" t) <> ".atom"

hashtagFileName :: TopicName -> FilePath
hashtagFileName t = Text.unpack t <> ".html"

hashtagAtomName :: TopicName -> FilePath
hashtagAtomName t = Text.unpack t <> ".atom"

data GenFileExtension
    = GenPngFile

extensionString :: GenFileExtension -> String
extensionString GenPngFile = ".png"

destTopic :: OutputPrefix -> TopicName -> DestinationLocation
destTopic prefix topic =
    VirtualFileDestination
        (Text.pack $ "/topics/" <> topicFileName topic)
        (prefix </> "topics" </> topicFileName topic)

destTopicAtom :: OutputPrefix -> TopicName -> DestinationLocation
destTopicAtom prefix topic =
    VirtualFileDestination
        (Text.pack $ "/topics/" <> topicAtomName topic)
        (prefix </> "topics" </> topicAtomName topic)

type HashTagName = Text.Text -- TODO: move me

destHashTag :: OutputPrefix -> HashTagName -> DestinationLocation
destHashTag prefix tag =
    VirtualFileDestination
        (Text.pack $ "/hashtags/" <> hashtagFileName tag)
        (prefix </> "hashtags" </> hashtagFileName tag)

destHashTagAtom :: OutputPrefix -> HashTagName -> DestinationLocation
destHashTagAtom prefix tag =
    VirtualFileDestination
        (Text.pack $ "/hashtags/" <> hashtagAtomName tag)
        (prefix </> "hashtags" </> hashtagAtomName tag)

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

destAudioFile :: OutputPrefix -> SourceLocation -> DestinationLocation
destAudioFile prefix (FileSource path) =
    StaticFileDestination
        (Text.pack $ "/audios/" <> takeFileName path)
        (prefix </> "audios" </> takeFileName path)

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

destDocumentFile :: OutputPrefix -> SourceLocation -> DestinationLocation
destDocumentFile prefix (FileSource path) =
    StaticFileDestination
        (Text.pack $ "/docs/" <> takeFileName path)
        (prefix </> "docs" </> takeFileName path)

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
