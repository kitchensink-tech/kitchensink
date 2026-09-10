module KitchenSink.Layout.Blog.Destinations
where

import Data.Text qualified as Text
import System.FilePath.Posix (takeBaseName, takeFileName, (</>))

import KitchenSink.Core.Build.Target (DestinationLocation (..), OutputPrefix, SourceLocation (..))
import KitchenSink.Core.Section (GeneratorInstructionsData (..))
import KitchenSink.Core.Section.Base as Core
import KitchenSink.Core.Section.Payloads (TopicName)
import KitchenSink.Prelude

-- | Root-relative URL prefix from site config (e.g. @"/tramaj"@, or @""@).
type UrlPrefix = Text.Text

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

-- | Prepends the configured site path-prefix (e.g. @"/tramaj"@, or @""@ when
-- hosted at the domain root) to a root-relative URL. Every @dest*@ function
-- below routes its 'Url' through this so a site hosted under a GitHub
-- Pages project subpath gets correct links.
withPrefix :: UrlPrefix -> String -> Text.Text
withPrefix prefix path = prefix <> Text.pack path

destTopic :: UrlPrefix -> OutputPrefix -> TopicName -> DestinationLocation
destTopic urlPrefix prefix topic =
    VirtualFileDestination
        (withPrefix urlPrefix $ "/topics/" <> topicFileName topic)
        (prefix </> "topics" </> topicFileName topic)

destTopicAtom :: UrlPrefix -> OutputPrefix -> TopicName -> DestinationLocation
destTopicAtom urlPrefix prefix topic =
    VirtualFileDestination
        (withPrefix urlPrefix $ "/topics/" <> topicAtomName topic)
        (prefix </> "topics" </> topicAtomName topic)

type HashTagName = Text.Text -- TODO: move me

destHashTag :: UrlPrefix -> OutputPrefix -> HashTagName -> DestinationLocation
destHashTag urlPrefix prefix tag =
    VirtualFileDestination
        (withPrefix urlPrefix $ "/hashtags/" <> hashtagFileName tag)
        (prefix </> "hashtags" </> hashtagFileName tag)

destHashTagAtom :: UrlPrefix -> OutputPrefix -> HashTagName -> DestinationLocation
destHashTagAtom urlPrefix prefix tag =
    VirtualFileDestination
        (withPrefix urlPrefix $ "/hashtags/" <> hashtagAtomName tag)
        (prefix </> "hashtags" </> hashtagAtomName tag)

destGenImage :: UrlPrefix -> OutputPrefix -> SourceLocation -> GenFileExtension -> DestinationLocation
destGenImage urlPrefix prefix (FileSource path) ext =
    StaticFileDestination
        (withPrefix urlPrefix $ "/gen/images/" <> takeFileName path <> extensionString ext)
        (prefix </> "gen/images" </> takeFileName path <> extensionString ext)

destGenArbitrary :: UrlPrefix -> OutputPrefix -> SourceLocation -> GeneratorInstructionsData -> DestinationLocation
destGenArbitrary urlPrefix prefix (FileSource path) g =
    StaticFileDestination
        (withPrefix urlPrefix $ "/gen/out/" <> takeFileName path <> "__" <> target g)
        (prefix </> "gen/out" </> takeFileName path <> "__" <> target g)

newtype FileExtension = FileExtension String

destinationExtension :: Format -> FileExtension
destinationExtension fmt = FileExtension $ case fmt of
    Core.Json -> "json"
    Core.Cmark -> "cmark"
    Core.Dhall -> "dhall"
    Core.Mustache -> "mustache"
    Core.TramajJson -> "tramaj-json"
    Core.TramajDoc -> "tramaj-doc"
    Core.TramajLib -> "tramaj-lib"
    Core.TextHtml -> "html"
    Core.Css -> "css"
    Core.Csv -> "csv"
    Core.InMemory -> "mem"

destEmbeddedData :: UrlPrefix -> OutputPrefix -> SourceLocation -> FileExtension -> Name -> Int -> DestinationLocation
destEmbeddedData urlPrefix prefix (FileSource path) (FileExtension ext) name index =
    StaticFileDestination
        (withPrefix urlPrefix $ "/raw/data/" <> takeFileName path <> "__" <> Text.unpack name <> show index <> "." <> ext)
        (prefix </> "raw/data" </> takeFileName path <> "__" <> Text.unpack name <> show index <> "." <> ext)

destVideoFile :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destVideoFile urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/videos/" <> takeFileName path)
        (prefix </> "videos" </> takeFileName path)

destAudioFile :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destAudioFile urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/audios/" <> takeFileName path)
        (prefix </> "audios" </> takeFileName path)

destRawFile :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destRawFile urlPrefix prefix (FileSource path)
    | takeFileName path == "robots.txt" =
        StaticFileDestination
            (withPrefix urlPrefix "/robots.txt")
            (prefix </> "robots.txt")
    | takeFileName path == "webfinger.json" =
        StaticFileDestination
            (withPrefix urlPrefix "/.well-known/webfinger")
            (prefix </> ".well-known" </> "webfinger")
    | otherwise =
        StaticFileDestination
            (withPrefix urlPrefix $ "/raw/" <> takeFileName path)
            (prefix </> "raw" </> takeFileName path)

destDocumentFile :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destDocumentFile urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/docs/" <> takeFileName path)
        (prefix </> "docs" </> takeFileName path)

destImage :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destImage urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/images/" <> takeFileName path)
        (prefix </> "images" </> takeFileName path)

destCssFile :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destCssFile urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/css/" <> takeFileName path)
        (prefix </> "css" </> takeFileName path)

destWebfontFile :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destWebfontFile urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/webfonts/" <> takeFileName path)
        (prefix </> "webfonts" </> takeFileName path)

destJsFile :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destJsFile urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/js/" <> takeFileName path)
        (prefix </> "js" </> takeFileName path)

destJsonDataFile :: UrlPrefix -> OutputPrefix -> FilePath -> DestinationLocation
destJsonDataFile urlPrefix prefix path =
    StaticFileDestination
        (withPrefix urlPrefix $ "/json/" <> takeFileName path)
        (prefix </> "json" </> takeFileName path)

destTextDataFile :: UrlPrefix -> OutputPrefix -> FilePath -> DestinationLocation
destTextDataFile urlPrefix prefix path =
    StaticFileDestination
        (withPrefix urlPrefix $ "/text/" <> takeFileName path)
        (prefix </> "text" </> takeFileName path)

destRootDataFile :: UrlPrefix -> OutputPrefix -> FilePath -> DestinationLocation
destRootDataFile urlPrefix prefix path =
    StaticFileDestination
        (withPrefix urlPrefix $ "/" <> takeFileName path)
        (prefix </> takeFileName path)

destHtml :: UrlPrefix -> OutputPrefix -> SourceLocation -> DestinationLocation
destHtml urlPrefix prefix (FileSource path) =
    StaticFileDestination
        (withPrefix urlPrefix $ "/" <> takeBaseName path <> ".html")
        (prefix </> takeBaseName path <> ".html")
