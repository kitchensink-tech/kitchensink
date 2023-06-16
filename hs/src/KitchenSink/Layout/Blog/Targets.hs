{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module KitchenSink.Layout.Blog.Targets (
    siteTargets
  , PathList
  , TargetType
  , PreambleSummary
  , TargetSummary
  , TopicSummary
  , GlossarySummary
  ) where

import GHC.Err (error)
import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Lucid (div_, id_, nav_, class_, article_)
import Data.Maybe (fromJust, catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.List as List
import qualified Data.ByteString.Lazy as LByteString

import Text.Feed.Types (Feed(AtomFeed))
import Text.XML (def, rsPretty)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeedWith)

import KitchenSink.Core.Build.Site (images, dotSourceFiles, videoFiles, rawFiles, cssFiles, jsFiles, htmlFiles, docFiles, articles)
import KitchenSink.Core.Build.Target (DestinationLocation, OutputPrefix, Sourced(..), SourceLocation(..), copyFrom, execCmd, destinationUrl, destination, summary, runAssembler)
import qualified KitchenSink.Core.Build.Target as Core
import KitchenSink.Core.Generator
import KitchenSink.Core.Section hiding (target)
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections
import KitchenSink.Layout.Blog.Extensions (ProductionRule, Assembler, Site, Article)
import qualified KitchenSink.Layout.Blog.Extensions as Ext
import KitchenSink.Layout.Blog.Destinations
import KitchenSink.Layout.Blog.Fragments
import KitchenSink.Layout.Blog.Analyses
import KitchenSink.Layout.Blog.Metadata
import KitchenSink.Layout.Blog.Summary
import KitchenSink.Layout.Blog.ArticleTypes
import KitchenSink.Layout.Blog.SpecialArticles as SpecialArticles

type Target = Ext.Target TargetSummary

target :: TargetSummary -> DestinationLocation -> ProductionRule -> Target
target z x y = Core.Target x y z

simpleTarget :: TargetType -> DestinationLocation -> ProductionRule -> Target
simpleTarget z x y = target (TargetSummary z Nothing Nothing Nothing Nothing Nothing) x y

imageTargets :: OutputPrefix -> Site -> [Target]
imageTargets prefix site =
  [ simpleTarget ImageTarget (destImage prefix loc) (copyFrom loc) | Sourced loc _ <- images site ]

dotimageTargets :: OutputPrefix -> Site -> [Target]
dotimageTargets prefix site =
  [ simpleTarget GraphVizImageTarget (destGenImage prefix loc GenPngFile) (execCmd "dot" ["-Tpng", "-o", "/dev/stdout", path] "") | Sourced loc@(FileSource path) _ <- dotSourceFiles site ]

videoTargets :: OutputPrefix -> Site -> [Target]
videoTargets prefix site =
  [ simpleTarget VideoTarget (destVideoFile prefix loc) (copyFrom loc) | Sourced loc _ <- videoFiles site ]

rawTargets :: OutputPrefix -> Site -> [Target]
rawTargets prefix site =
  [ simpleTarget RawTarget (destRawFile prefix loc) (copyFrom loc) | Sourced loc _ <- rawFiles site ]

documentTargets :: OutputPrefix -> Site -> [Target]
documentTargets prefix site =
  [ simpleTarget DocumentTarget (destDocumentFile prefix loc) (copyFrom loc) | Sourced loc _ <- docFiles site ]

cssTargets :: OutputPrefix -> Site -> [Target]
cssTargets prefix site =
  [ simpleTarget CssTarget (destCssFile prefix loc) (copyFrom loc) | Sourced loc _ <- cssFiles site ]

jsTargets :: OutputPrefix -> Site -> [Target]
jsTargets prefix site =
  [ simpleTarget JavaScriptSourceTarget (destJsFile prefix loc) (copyFrom loc) | Sourced loc _ <- jsFiles site ]

htmlTargets :: OutputPrefix -> Site -> [Target]
htmlTargets prefix site =
  [ simpleTarget HtmlSourceTarget (destHtml prefix loc) (copyFrom loc) | Sourced loc _ <- htmlFiles site ]

jsonDataTarget :: ToJSON a => OutputPrefix -> a -> FilePath -> Target
jsonDataTarget prefix v loc =
  simpleTarget JSONTarget (destJsonDataFile prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ LByteString.toStrict $ encode v

rootDataTarget :: OutputPrefix -> Text -> FilePath -> Target
rootDataTarget prefix v loc =
  simpleTarget RootFileTarget (destRootDataFile prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ Text.encodeUtf8 v

siteTargets :: OutputPrefix -> MetaData -> Site -> [Target]
siteTargets prefix extra site = allTargets
  where
    allTargets = mconcat
      [ embeddedGeneratorTargets
      , embeddedDataTargets
      , fmap fst articleTargets
      , imageTargets prefix site
      , dotimageTargets prefix site
      , videoTargets prefix site
      , rawTargets prefix site
      , documentTargets prefix site
      , cssTargets prefix site
      , jsTargets prefix site
      , htmlTargets prefix site
      , topicIndexesTargets (lookupSpecialArticle SpecialArticles.Topics site)
      , topicAtomTargets (lookupSpecialArticle SpecialArticles.Topics site)
      , hashtagIndexesTargets (lookupSpecialArticle SpecialArticles.HashTagListings site)
      , hashtagAtomTargets (lookupSpecialArticle SpecialArticles.Topics site)
      , glossaryTargets (lookupSpecialArticleSource SpecialArticles.Glossary site)
      , jsonDataTargets
      , seoTargets
      ]

    pathList :: PathList
    pathList = PathList $ [ (destinationUrl (destination tgt), summary tgt) | tgt <- allTargets ]

    jsonDataTargets :: [Target]
    jsonDataTargets =
      [ jsonDataTarget prefix (pathList) "paths.json"
      , jsonDataTarget prefix (filecounts site) "filecounts.json"
      , jsonDataTarget prefix (topicsgraph (ExternalSitesInfo $ externalKitchenSinkURLs extra) stats) "topicsgraph.json"
      ] <> [ jsonDataTarget prefix (analyzeArticle art) (p <> ".json") | (Sourced (FileSource p) art) <- articles site
      ]

    seoTargets :: [Target]
    seoTargets =
      [ rootDataTarget prefix (Text.unlines $ fmap (\x -> publishBaseURL extra <> x) $ fmap (destinationUrl . destination . fst) articleTargets) "sitemap.txt"
      , rootDataTarget prefix (atomFeedContent articleTargets) "atom.xml"
      ]
 
    atomFeedContent :: [(Ext.Target z, Article [Text])] -> Text
    atomFeedContent targets =
      let render = LText.toStrict . fromJust . Export.textFeedWith def{rsPretty=True} . AtomFeed
          uri = publishBaseURL extra <> (destinationUrl $ destRootDataFile prefix "atom.xml")
      in render
         $ feedForArticles uri
         $ List.filter (isPublishedArticle . snd)
         $ List.filter (isListableArticle . snd)
         $ targets

    feedForArticles :: Atom.URI -> [(Ext.Target z, Article [Text])] -> Atom.Feed
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

    toEntry :: (Ext.Target z, Article [Text]) -> Assembler Atom.Entry
    toEntry (tgt, art) = assembleAtomEntry extra (destination tgt) art

    articleTarget :: Sourced (Article [Text]) -> Target
    articleTarget (Sourced loc@(FileSource path) art) =
      let u = destHtml prefix loc
          j = destJsonDataFile prefix (path <> ".json") -- todo:unify
          tgtSummary = TargetSummary ArticleTarget (articleTitle art) (articleCompactSummary art) (summarizePreamble <$> articlePreambleData art) (summarizeTopic <$> articleTopicData art) (summarizeGlossary <$> articleGlossaryData art)
      in target tgtSummary u (Core.ProduceAssembler $ layoutFor u j art)

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
        getTargets loc art =
          either (error . show) (fmap (generatorTarget loc))
          $ runAssembler
          $ generatorInstructions art

        generatorTarget :: SourceLocation -> GeneratorInstructionsData -> Target
        generatorTarget loc g =
          let rule = execCmd (Text.unpack $ cmd g) (fmap Text.unpack $ args g) (maybe "" Text.encodeUtf8 $ stdin g)
          in simpleTarget GeneratedTarget (destGenArbitrary prefix loc g) rule

        generatorInstructions :: Article [Text] -> Assembler [GeneratorInstructionsData]
        generatorInstructions art =
          getSections art GeneratorInstructions
            >>= traverse (fmap extract . jsonSection)

    embeddedDataTargets :: [Target]
    embeddedDataTargets =
      [ tgt
      | Sourced loc art <- articles site
      , tgt <- getTargets loc art
      ]
      where
        getTargets :: SourceLocation -> Article [Text] -> [Target]
        getTargets loc art =
          either (error . show) (fmap (dataTarget loc))
          $ runAssembler
          $ datasets art

        dataTarget :: SourceLocation -> (Int, Section () [Text]) -> Target
        dataTarget loc (index, (Section _ format contents)) =
          let 
            dataDestination = destEmbeddedData prefix loc (destinationExtension format) index
            rule = Core.ProduceAssembler (pure $ LText.fromStrict $ Text.unlines contents)
          in
          simpleTarget DatasetTarget dataDestination rule

        datasets :: Article [Text] -> Assembler [(Int,Section () [Text])]
        datasets art = do
          sections <- getSections art Dataset
          pure $ List.zip [1..] sections

    topicIndexesTargets :: Maybe (Article [Text]) -> [ Target ]
    topicIndexesTargets Nothing = []
    topicIndexesTargets (Just art) =
      [ let u = destTopic prefix topic
        in simpleTarget TopicsIndexTarget u (Core.ProduceAssembler $ topicsLayout topic articles u u art)
      | (topic, articles) <- Map.toList (byTopic stats)
      ]

    topicAtomTargets :: Maybe (Article [Text]) -> [ Target ]
    topicAtomTargets Nothing = []
    topicAtomTargets (Just _) =
      [ let u = destTopicAtom prefix topic
            rule = Core.ProduceAssembler $ pure $ LText.fromStrict $ atomFeedContent articles
        in simpleTarget TopicsIndexTarget u rule
      | (topic, articles) <- Map.toList (byTopic stats)
      ]

    hashtagIndexesTargets :: Maybe (Article [Text]) -> [ Target ]
    hashtagIndexesTargets Nothing = []
    hashtagIndexesTargets (Just art) =
      [ let u = destHashTag prefix (hashtagValue tag)
        in simpleTarget HashTagsIndexTarget u (Core.ProduceAssembler $ hashtagsLayout tag articles u u art)
      | (tag, articles) <- Map.toList (byHashTag stats)
      ]

    hashtagAtomTargets :: Maybe (Article [Text]) -> [ Target ]
    hashtagAtomTargets Nothing = []
    hashtagAtomTargets (Just _) =
      [ let u = destHashTagAtom prefix (hashtagValue tag)
            rule = Core.ProduceAssembler $ pure $ LText.fromStrict $ atomFeedContent articles
        in simpleTarget HashTagsIndexTarget u rule
      | (tag, articles) <- Map.toList (byHashTag stats)
      ]

    glossaryTargets :: Maybe (Sourced (Article [Text])) -> [ Target ]
    glossaryTargets Nothing = []
    glossaryTargets (Just (Sourced loc art)) =
      let u = destHtml prefix loc
      in
      [ simpleTarget GlossaryTarget u (Core.ProduceAssembler $ glossaryListingLayout articleTargets u u art)
      ]

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
      , (ArchivedArticle, archivedArticleLayout)
      , (UpcomingArticle, upcomingArticleLayout)
      , (PublishedArticle, articleLayout)
      , (IndexPage, indexLayout)
      ]

    stats :: TopicStats
    stats = buildTopicStats (articles site) (fmap (const ()) . articleTarget)

    wholeGlossary :: WholeGlossary
    wholeGlossary = buildWholeGlossary (articles site) (fmap (const ()) . articleTarget)

    rootAtomDLoc :: DestinationLocation
    rootAtomDLoc = destRootDataFile prefix "atom.xml"

    indexLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    indexLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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

    archivedArticleLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    archivedArticleLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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
                        , assembleArchivedMain
                        , assembleFooter
                        ]
                      ]
                  ]

    upcomingArticleLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    upcomingArticleLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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
                        , assembleGlossary
                        , assembleFooter
                        ]
                      ]
                  ]

    spaLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    spaLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ id_ "listing" ])
                      $ mconcat
                        [ assembleMain
                        ]
                      ]
                  ]


    topicsLayout :: TopicName -> [ (Ext.Target a, Article [Text]) ] -> DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    topicsLayout topic articles dloc jsondloc =
      let atomDLoc = destTopicAtom prefix topic in
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc atomDLoc) assembleStyle
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
                        [ const (assembleTopicListing prefix stats topic articles)
                        ]
                      ]
                  ]

    hashtagsLayout :: HashTagInfo -> [ (Ext.Target a, Article [Text]) ] -> DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    hashtagsLayout tag articles dloc jsondloc =
      let atomDLoc = destHashTagAtom prefix (hashtagValue tag) in
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc atomDLoc) assembleStyle
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
                        [ const (assembleHashtagListing (hashtagValue tag) articles)
                        ]
                      ]
                  ]

    glossaryListingLayout :: [ (Ext.Target a, Article [Text]) ] -> DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    glossaryListingLayout articles dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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
                        [ const (assembleGlossaryListing prefix wholeGlossary articles)
                        ]
                      ]
                  ]


    -- TODO: add warning on default layout
    defaultLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    defaultLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead (MetaHeaders extra dloc jsondloc rootAtomDLoc) assembleStyle
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
                        , assembleGlossary
                        , assembleFooter
                        ]
                      ]
                  ]

