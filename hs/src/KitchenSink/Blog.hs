{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module KitchenSink.Blog (
    siteTargets
  , PathList
  , TargetType
  , PreambleSummary
  , TargetSummary
  , TopicSummary
  ) where

import GHC.Err (error)
import Data.Aeson (ToJSON, FromJSON, encode)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lucid as Lucid
import Data.Maybe (fromJust, catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.List as List
import System.FilePath.Posix (takeFileName)
import qualified Data.ByteString.Lazy as LByteString

import Text.Feed.Types (Feed(AtomFeed))
import Text.XML (def, rsPretty)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeedWith)

import KitchenSink.Blog.Target hiding (Target)
import qualified KitchenSink.Blog.Target as BlogTarget
import KitchenSink.Blog.Generator
import KitchenSink.Blog.Section
import KitchenSink.Blog.Site
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Basics hiding (PreambleData, target)
import qualified KitchenSink.Blog.Basics as Basics
import KitchenSink.Blog.Layout
import KitchenSink.Blog.AssembleSections
import KitchenSink.Blog.Advanced

data TargetType
  = CssTarget
  | ImageTarget
  | GraphVizImageTarget
  | VideoTarget
  | RawTarget
  | JavaScriptSourceTarget
  | JSONTarget
  | RootFileTarget
  | ArticleTarget
  | GeneratedTarget
  | TopicsIndexTarget
  deriving (Show, Generic)
instance ToJSON TargetType
instance FromJSON TargetType

data PreambleSummary = PreambleSummary {
    author  :: Text
  , datetxt :: Maybe Text
  , title   :: Text
  , faviconUrl   :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON PreambleSummary
instance ToJSON PreambleSummary

data TopicSummary = TopicSummary {
    tags :: [Text]
  , keywords :: [Text]
  , imageLink :: Maybe Text
  } deriving (Show, Eq, Generic)
instance FromJSON TopicSummary
instance ToJSON TopicSummary

summarizePreamble :: Basics.PreambleData -> PreambleSummary
summarizePreamble p =
  PreambleSummary
    (Basics.author p)
    (Basics.datetxt p)
    (Basics.title p)
    (fromMaybe defaultFavicon $ Basics.faviconUrl p)

summarizeTopic :: TopicData -> TopicSummary
summarizeTopic (TopicData{..}) = TopicSummary{..}

data TargetSummary
  = TargetSummary
  { targetType :: TargetType
  , textualTitle :: Maybe Text
  , textualSummary :: Maybe Text
  , preambleSummary :: Maybe PreambleSummary
  , topicSummary :: Maybe TopicSummary
  } deriving (Show, Generic)
instance ToJSON TargetSummary
instance FromJSON TargetSummary

data PathList = PathList {
    paths :: [(Text, TargetSummary)]
  }
  deriving (Show, Generic)
instance ToJSON PathList
instance FromJSON PathList

type Target = BlogTarget.Target TargetSummary

target :: TargetSummary -> DestinationLocation -> ProductionRule -> Target
target z x y = BlogTarget.Target x y z

simpleTarget :: TargetType -> DestinationLocation -> ProductionRule -> Target
simpleTarget z x y = target (TargetSummary z Nothing Nothing Nothing Nothing) x y

imageTargets :: OutputPrefix -> Site -> [Target]
imageTargets prefix site =
  [ simpleTarget ImageTarget (destImage prefix loc) (copyFrom loc) | Sourced loc _ <- images site ]

dotimageTargets :: OutputPrefix -> Tracer -> Site -> [Target]
dotimageTargets prefix trace site =
  [ simpleTarget GraphVizImageTarget (destGenImage prefix loc GenPngFile) (execCmd trace "dot" ["-Tpng", "-o", "/dev/stdout", path] "") | Sourced loc@(FileSource path) _ <- dotSourceFiles site ]

videoTargets :: OutputPrefix -> Site -> [Target]
videoTargets prefix site =
  [ simpleTarget VideoTarget (destVideoFile prefix loc) (copyFrom loc) | Sourced loc _ <- videoFiles site ]

rawTargets :: OutputPrefix -> Site -> [Target]
rawTargets prefix site =
  [ simpleTarget RawTarget (destRawFile prefix loc) (copyFrom loc) | Sourced loc _ <- rawFiles site ]

cssTargets :: OutputPrefix -> Site -> [Target]
cssTargets prefix site =
  [ simpleTarget CssTarget (destCssFile prefix loc) (copyFrom loc) | Sourced loc _ <- cssFiles site ]

jsTargets :: OutputPrefix -> Site -> [Target]
jsTargets prefix site =
  [ simpleTarget JavaScriptSourceTarget (destJsFile prefix loc) (copyFrom loc) | Sourced loc _ <- jsFiles site ]

jsonDataTarget :: ToJSON a => OutputPrefix -> a -> FilePath -> Target
jsonDataTarget prefix v loc =
  simpleTarget JSONTarget (destJsonDataFile prefix loc) (ProduceGenerator $ Generator $ pure $ Right $ LByteString.toStrict $ encode v)

rootDataTarget :: OutputPrefix -> Text -> FilePath -> Target
rootDataTarget prefix v loc =
  simpleTarget RootFileTarget (destRootDataFile prefix loc) (ProduceGenerator $ Generator $ pure $ Right $ Text.encodeUtf8 v)

siteTargets :: OutputPrefix -> Tracer -> MetaExtraData -> Site -> [Target]
siteTargets prefix tracer extra site = allTargets
  where
    allTargets = mconcat
      [ embeddedGeneratorTargets
      , fmap fst articleTargets
      , imageTargets prefix site
      , dotimageTargets prefix tracer site
      , videoTargets prefix site
      , rawTargets prefix site
      , cssTargets prefix site
      , jsTargets prefix site
      , tagIndexesTargets (lookupSpecialArticle "tags.cmark" site)
      , jsonDataTargets
      , seoTargets
      ]

    pathList :: PathList
    pathList = PathList $ [ (destinationUrl (destination tgt), summary tgt) | tgt <- allTargets ]

    jsonDataTargets :: [Target]
    jsonDataTargets =
      [ jsonDataTarget prefix (pathList) "paths.json"
      , jsonDataTarget prefix (filecounts site) "filecounts.json"
      , jsonDataTarget prefix (topicsgraph stats) "topicsgraph.json"
      ] <> [ jsonDataTarget prefix (analyzeArticle art) (p <> ".json") | (Sourced (FileSource p) art) <- articles site
      ]

    seoTargets :: [Target]
    seoTargets =
      [ rootDataTarget prefix (Text.unlines $ fmap (\x -> publishBaseURL extra <> x) $ fmap (destinationUrl . destination . fst) articleTargets) "sitemap.txt"
      , rootDataTarget prefix atomRootFeedContent "atom.xml"
      ]
 
    atomRootFeedContent :: Text
    atomRootFeedContent =
      let render = LText.toStrict . fromJust . Export.textFeedWith def{rsPretty=True} . AtomFeed
          uri = publishBaseURL extra <> (destinationUrl $ destRootDataFile prefix "atom.xml")
      in render
         $ feedForArticles uri
         $ List.filter (isPublishedArticle . snd)
         $ List.filter (isListableArticle . snd)
         $ articleTargets

    feedForArticles :: Atom.URI -> [(Target, Article [Text])] -> Atom.Feed
    feedForArticles uri arts =
      let baseFeed = Atom.nullFeed uri (Atom.TextString $ baseTitle extra) updatedAt
          fmtUTC = Text.pack . iso8601Show
          -- picks the first date in the article list, recall that date is optional
          updatedAt = fmtUTC
                      $ fromMaybe epochUTCTime
                      $ listToMaybe
                      $ catMaybes
                      $ fmap (extractDate . snd)
                      $ arts
          mkfeed xs = baseFeed { Atom.feedEntries = xs }
          entries = traverse toEntry
                  $ sortByDate arts
      in case runAssembler (mkfeed <$> entries) of
            Left err -> error (show err)
            Right x -> x

    toEntry :: (Target, Article [Text]) -> Assembler Atom.Entry
    toEntry (tgt, art) = assembleAtomEntry extra (destination tgt) art

    articleTarget :: Sourced (Article [Text]) -> Target
    articleTarget (Sourced loc@(FileSource path) art) =
      let u = destHtml prefix loc
          j = destJsonDataFile prefix (path <> ".json") -- todo:unify
          tgtSummary = TargetSummary ArticleTarget (articleTitle art) (articleCompactSummary art) (summarizePreamble <$> articlePreambleData art) (summarizeTopic <$> articleTopicData art)
      in target tgtSummary u (ProduceAssembler $ layoutFor u j art)

    articleTargets :: [(Target, Article [Text])]
    articleTargets =
      [ (articleTarget srca, obj srca)
      | srca <- articles site
      , isConcreteTarget $ obj srca
      ]

    -- TODO: raise errors here
    embeddedGeneratorTargets :: [Target]
    embeddedGeneratorTargets =
      [ tgt
      | Sourced loc art <- articles site
      , tgt <- getTargets loc art
      ]
      where
        getTargets :: SourceLocation -> Article [Text] -> [Target]
        getTargets loc art = either (error . show) (fmap (genTarget loc)) . runAssembler $ (f art)

        genTarget :: SourceLocation -> GeneratorInstructionsData -> Target
        genTarget loc g = let rule = execCmd tracer (Text.unpack $ cmd g) (fmap Text.unpack $ args g) (maybe "" Text.encodeUtf8 $ stdin g)
                      in simpleTarget GeneratedTarget (destGenArbitrary prefix loc g) rule

        f :: Article [Text] -> Assembler [GeneratorInstructionsData]
        f art = getSections art GeneratorInstructions
            >>= traverse (fmap extract . jsonSection)

    tagIndexesTargets :: Maybe (Article [Text]) -> [ Target ]
    tagIndexesTargets Nothing = []
    tagIndexesTargets (Just art) = [ let u = destTag prefix tag in simpleTarget TopicsIndexTarget u (ProduceAssembler $ tagsLayout tag u u art) | tag <- Map.keys $ byTopic stats ]

    layoutFor :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    layoutFor dloc jsondloc art =
      let go = fromMaybe defaultLayout
             $ flip List.lookup layoutMap
             $ layoutNameFor art
      in go dloc jsondloc art

    layoutMap :: [(ArticleLayout, DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text)]
    layoutMap =
      [ (SinglePageApp, spaLayout)
      , (VariousListing, variousListingLayout)
      , (ImageGallery, imageGalleryLayout)
      , (UpcomingArticle, upcomingArticleLayout)
      , (PublishedArticle, articleLayout)
      , (IndexPage, indexLayout)
      ]

    stats :: TopicStats
    stats = buildTopicStats (articles site) (fmap (const ()) . articleTarget )

    indexLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    indexLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc
                    $ mconcat
                      [ assembleStyle
                      ]
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleMain
                        , const $ pure $ latestArticleLink articleTargets
                        , const $ pure $ siteGraphEchartZone
                        , const $ pure $ mainArticleLinks articleTargets
                        , const $ pure $ topicsListings stats
                        ]
                      ]
                  ]

    upcomingArticleLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    upcomingArticleLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleHeader prefix stats dloc
                        , assembleUpcomingMain
                        , assembleFooter
                        ]
                      ]
                  ]

    articleLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    articleLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleHeader prefix stats dloc
                        , assembleMain
                        , assembleFooter
                        ]
                      ]
                  ]

    spaLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    spaLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ id_ "spa", class_ "application"]) mempty
                      , wrap (div_ [ class_ "help"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleMain
                        ]
                      ]
                  ]

    imageGalleryLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    imageGalleryLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ id_ "gallery", class_ "photos"])
                      $ mconcat
                        [ assembleMain
                        ]
                      ]
                  ]

    variousListingLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    variousListingLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ id_ "listing" ])
                      $ mconcat
                        [ assembleMain
                        ]
                      ]
                  ]


    tagsLayout :: Tag -> DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    tagsLayout tag dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ const (assembleTopicListing prefix stats tag)
                        ]
                      ]
                  ]

    -- TODO: add warning on default layout
    defaultLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    defaultLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleHeader prefix stats dloc
                        , assembleDefaultLayoutWarning
                        , assembleMain
                        , assembleFooter
                        ]
                      ]
                  ]

lookupSpecialArticle :: Text -> Site -> Maybe (Article [Text])
lookupSpecialArticle name site = obj <$> List.find f (articles site)
  where
    f :: Sourced a -> Bool
    f (Sourced (FileSource path) _) = takeFileName path == Text.unpack name
