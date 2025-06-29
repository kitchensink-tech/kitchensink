{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KitchenSink.Layout.Blog.Targets (
    siteTargets,
    PathList,
    TargetType,
    PreambleSummary,
    TargetSummary,
    TopicSummary,
    GlossarySummary,
) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as LByteString
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Err (error)
import Lucid (article_, class_, div_, id_, nav_)

import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Export (textFeedWith)
import Text.Feed.Types (Feed (AtomFeed))
import Text.XML (def)

import KitchenSink.Core.Assembler.Sections
import KitchenSink.Core.Build.Site (articles, audioFiles, cssFiles, docFiles, dotSourceFiles, htmlFiles, images, jsFiles, rawFiles, videoFiles, webfontFiles)
import KitchenSink.Core.Build.Target (DestinationLocation, ExecRoot, OutputPrefix, SourceLocation (..), Sourced (..), copyFrom, destination, destinationUrl, execCmd, runAssembler, summary)
import KitchenSink.Core.Build.Target qualified as Core
import KitchenSink.Core.Generator
import KitchenSink.Core.Section hiding (target)
import KitchenSink.Layout.Blog.Analyses
import KitchenSink.Layout.Blog.Analyses.TextRender qualified as TextRender
import KitchenSink.Layout.Blog.ArticleTypes
import KitchenSink.Layout.Blog.Destinations
import KitchenSink.Layout.Blog.Extensions (Article, Assembler, ProductionRule, Site)
import KitchenSink.Layout.Blog.Extensions qualified as Ext
import KitchenSink.Layout.Blog.Fragments
import KitchenSink.Layout.Blog.Metadata
import KitchenSink.Layout.Blog.SpecialArticles as SpecialArticles
import KitchenSink.Layout.Blog.Summary
import KitchenSink.Prelude

type Target = Ext.Target TargetSummary

target :: TargetSummary -> DestinationLocation -> ProductionRule -> Target
target z x y = Core.Target x y z

simpleTarget :: TargetType -> DestinationLocation -> ProductionRule -> Target
simpleTarget z x y =
    target (TargetSummary z Nothing Nothing Nothing Nothing Nothing (HashTagSummary [])) x y

imageTargets :: OutputPrefix -> Site -> [Target]
imageTargets prefix site =
    [simpleTarget ImageTarget (destImage prefix loc) (copyFrom loc) | Sourced loc _ <- site.images]

dotimageTargets :: OutputPrefix -> Site -> [Target]
dotimageTargets prefix site =
    [ simpleTarget
        GraphVizImageTarget
        (destGenImage prefix loc GenPngFile)
        (execCmd root "dot" ["-Tpng", "-o", "/dev/stdout", path] "")
    | Sourced loc@(FileSource path) _ <- site.dotSourceFiles
    ]
  where
    root :: ExecRoot
    root = Nothing

videoTargets :: OutputPrefix -> Site -> [Target]
videoTargets prefix site =
    [simpleTarget VideoTarget (destVideoFile prefix loc) (copyFrom loc) | Sourced loc _ <- site.videoFiles]

audioTargets :: OutputPrefix -> Site -> [Target]
audioTargets prefix site =
    [simpleTarget AudioTarget (destAudioFile prefix loc) (copyFrom loc) | Sourced loc _ <- site.audioFiles]

rawTargets :: OutputPrefix -> Site -> [Target]
rawTargets prefix site =
    [simpleTarget RawTarget (destRawFile prefix loc) (copyFrom loc) | Sourced loc _ <- site.rawFiles]

documentTargets :: OutputPrefix -> Site -> [Target]
documentTargets prefix site =
    [simpleTarget DocumentTarget (destDocumentFile prefix loc) (copyFrom loc) | Sourced loc _ <- site.docFiles]

cssTargets :: OutputPrefix -> Site -> [Target]
cssTargets prefix site =
    [simpleTarget CssTarget (destCssFile prefix loc) (copyFrom loc) | Sourced loc _ <- site.cssFiles]

webfontTargets :: OutputPrefix -> Site -> [Target]
webfontTargets prefix site =
    [simpleTarget WebfontTarget (destWebfontFile prefix loc) (copyFrom loc) | Sourced loc _ <- site.webfontFiles]

jsTargets :: OutputPrefix -> Site -> [Target]
jsTargets prefix site =
    [simpleTarget JavaScriptSourceTarget (destJsFile prefix loc) (copyFrom loc) | Sourced loc _ <- site.jsFiles]

htmlTargets :: OutputPrefix -> Site -> [Target]
htmlTargets prefix site =
    [simpleTarget HtmlSourceTarget (destHtml prefix loc) (copyFrom loc) | Sourced loc _ <- site.htmlFiles]

jsonDataTarget :: (ToJSON a) => OutputPrefix -> a -> FilePath -> Target
jsonDataTarget prefix v loc =
    simpleTarget JSONTarget (destJsonDataFile prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ LByteString.toStrict $ encode v

textDataTarget :: OutputPrefix -> Article [Text] -> FilePath -> Target
textDataTarget prefix v loc =
    simpleTarget JSONTarget (destTextDataFile prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ Text.encodeUtf8 $ TextRender.textRender v

rootDataTarget :: OutputPrefix -> Text -> FilePath -> Target
rootDataTarget prefix v loc =
    simpleTarget RootFileTarget (destRootDataFile prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ Text.encodeUtf8 v

siteTargets :: ExecRoot -> OutputPrefix -> MetaData -> Site -> [Target]
siteTargets execRoot prefix extra site = allTargets
  where
    allTargets =
        mconcat
            [ embeddedGeneratorTargets
            , embeddedDataTargets
            , fmap fst articleTargets
            , imageTargets prefix site
            , dotimageTargets prefix site
            , videoTargets prefix site
            , audioTargets prefix site
            , rawTargets prefix site
            , documentTargets prefix site
            , cssTargets prefix site
            , webfontTargets prefix site
            , jsTargets prefix site
            , htmlTargets prefix site
            , topicIndexesTargets (lookupSpecialArticle SpecialArticles.Topics site)
            , topicAtomTargets (lookupSpecialArticle SpecialArticles.Topics site)
            , hashtagIndexesTargets (lookupSpecialArticle SpecialArticles.HashTagListings site)
            , hashtagAtomTargets (lookupSpecialArticle SpecialArticles.Topics site)
            , glossaryTargets (lookupSpecialArticleSource SpecialArticles.Glossary site)
            , jsonDataTargets
            , textDataTargets
            , seoTargets
            ]

    pathList :: PathList
    pathList = PathList $ [(destinationUrl (destination tgt), summary tgt) | tgt <- allTargets]

    jsonDataTargets :: [Target]
    jsonDataTargets =
        [ jsonDataTarget prefix (pathList) "paths.json"
        , jsonDataTarget prefix (filecounts site) "filecounts.json"
        , jsonDataTarget prefix (topicsgraph (ExternalSitesInfo $ externalKitchenSinkURLs extra) stats) "topicsgraph.json"
        ]
            <> [ jsonDataTarget prefix (analyzeArticle art) (p <> ".json") | (Sourced (FileSource p) art) <- site.articles
               ]

    textDataTargets :: [Target]
    textDataTargets =
        [ textDataTarget prefix art (p <> ".text") | (Sourced (FileSource p) art) <- site.articles
        ]

    seoTargets :: [Target]
    seoTargets =
        [ rootDataTarget prefix (Text.unlines $ fmap (\x -> publishBaseURL extra <> x) $ fmap (destinationUrl . destination . fst) articleTargets) "sitemap.txt"
        , rootDataTarget prefix (atomFeedContent articleTargets) "atom.xml"
        ]

    atomFeedContent :: [(Ext.Target z, Article [Text])] -> Text
    atomFeedContent targets =
        let render = LText.toStrict . fromJust . Export.textFeedWith def . AtomFeed
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
            updatedAt =
                fmtUTC
                    $ fromMaybe epochUTCTime
                    $ listToMaybe
                    $ catMaybes
                    $ fmap (extractDate . snd)
                    $ arts
            mkfeed xs = baseFeed{Atom.feedEntries = xs}
            entries =
                traverse toEntry
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
            t = destTextDataFile prefix (path <> ".text") -- todo:unify
            tgtSummary = TargetSummary ArticleTarget (articleTitle art) (articleCompactSummary art) (summarizePreamble <$> articlePreambleData art) (summarizeTopic <$> articleTopicData art) (summarizeGlossary <$> articleGlossaryData art) (summarizeHashTags $ analyzeArticle art)
         in target tgtSummary u (Core.ProduceAssembler $ layoutFor u j t art)

    articleTargets :: [(Target, Article [Text])]
    articleTargets =
        [ (articleTarget srca, srca.obj)
        | srca <- site.articles
        , isConcreteTarget srca.obj
        ]

    -- TODO: raise errors here
    embeddedGeneratorTargets :: [Target]
    embeddedGeneratorTargets =
        [ tgt
        | Sourced loc art <- site.articles
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
            let rule =
                    execCmd
                        execRoot
                        (Text.unpack g.cmd)
                        (fmap Text.unpack g.args)
                        (fromMaybe "" $ (fmap Text.encodeUtf8 g.stdin) <|> (fmap (LByteString.toStrict . encode) g.stdin_json))
             in simpleTarget GeneratedTarget (destGenArbitrary prefix loc g) rule

        generatorInstructions :: Article [Text] -> Assembler [GeneratorInstructionsData]
        generatorInstructions art =
            getSections art isGeneratorInstructions
                >>= traverse (fmap extract . jsonSection)

    embeddedDataTargets :: [Target]
    embeddedDataTargets =
        [ tgt
        | Sourced loc art <- site.articles
        , tgt <- getTargets loc art
        ]
      where
        getTargets :: SourceLocation -> Article [Text] -> [Target]
        getTargets loc art =
            catMaybes
                $ either (error . show) (fmap (dataTarget loc)) -- note catMaybes won't swallow errors as `datasets` is a morally-correct filter for valid `dataTarget` arguments
                $ runAssembler
                $ datasets art

        dataTarget :: SourceLocation -> (Int, Section () [Text]) -> Maybe Target
        dataTarget loc (index, (Section (Dataset name) format contents)) =
            let
                dataDestination = destEmbeddedData prefix loc (destinationExtension format) name index
                rule = Core.ProduceAssembler (pure $ LText.fromStrict $ Text.unlines contents)
             in
                Just $ simpleTarget DatasetTarget dataDestination rule
        dataTarget _ _ = Nothing -- return Nothing on non-Data item only
        datasets :: Article [Text] -> Assembler [(Int, Section () [Text])]
        datasets art = do
            sections <- getSections art isDataset
            pure $ List.zip [1 ..] sections

    topicIndexesTargets :: Maybe (Article [Text]) -> [Target]
    topicIndexesTargets Nothing = []
    topicIndexesTargets (Just art) =
        [ let u = destTopic prefix topic
           in simpleTarget TopicsIndexTarget u (Core.ProduceAssembler $ topicsLayout topic articles u u u art)
        | (topic, articles) <- Map.toList (byTopic stats)
        ]

    topicAtomTargets :: Maybe (Article [Text]) -> [Target]
    topicAtomTargets Nothing = []
    topicAtomTargets (Just _) =
        [ let u = destTopicAtom prefix topic
              rule = Core.ProduceAssembler $ pure $ LText.fromStrict $ atomFeedContent articles
           in simpleTarget TopicsIndexTarget u rule
        | (topic, articles) <- Map.toList (byTopic stats)
        ]

    hashtagIndexesTargets :: Maybe (Article [Text]) -> [Target]
    hashtagIndexesTargets Nothing = []
    hashtagIndexesTargets (Just art) =
        [ let u = destHashTag prefix (hashtagValue tag)
           in simpleTarget HashTagsIndexTarget u (Core.ProduceAssembler $ hashtagsLayout tag articles u u u art)
        | (tag, articles) <- Map.toList (byHashTag stats)
        ]

    hashtagAtomTargets :: Maybe (Article [Text]) -> [Target]
    hashtagAtomTargets Nothing = []
    hashtagAtomTargets (Just _) =
        [ let u = destHashTagAtom prefix (hashtagValue tag)
              rule = Core.ProduceAssembler $ pure $ LText.fromStrict $ atomFeedContent articles
           in simpleTarget HashTagsIndexTarget u rule
        | (tag, articles) <- Map.toList (byHashTag stats)
        ]

    glossaryTargets :: Maybe (Sourced (Article [Text])) -> [Target]
    glossaryTargets Nothing = []
    glossaryTargets (Just (Sourced loc art)) =
        let u = destHtml prefix loc
         in [ simpleTarget GlossaryTarget u (Core.ProduceAssembler $ glossaryListingLayout articleTargets u u u art)
            ]

    layoutFor ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    layoutFor dloc jsondloc txtdloc art =
        let go =
                fromMaybe defaultLayout
                    $ flip List.lookup layoutMap
                    $ layoutNameFor art
         in go dloc jsondloc txtdloc art

    layoutMap :: [(ArticleLayout, DestinationLocation -> DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text)]
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
    stats = buildTopicStats site.articles (fmap (const ()) . articleTarget)

    wholeGlossary :: WholeGlossary
    wholeGlossary = buildWholeGlossary site.articles (fmap (const ()) . articleTarget)

    rootAtomDLoc :: DestinationLocation
    rootAtomDLoc = destRootDataFile prefix "atom.xml"

    indexLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    indexLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (div_ [class_ "main"])
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

    archivedArticleLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    archivedArticleLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                            $ mconcat
                                [ const $ pure $ homeLink
                                , const $ pure $ searchBox
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleHeader prefix stats dloc
                                , assembleArchivedMain
                                , assembleFooter
                                ]
                        ]
                ]

    upcomingArticleLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    upcomingArticleLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                            $ mconcat
                                [ const $ pure $ homeLink
                                , const $ pure $ searchBox
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleHeader prefix stats dloc
                                , assembleUpcomingMain
                                , assembleFooter
                                ]
                        ]
                ]

    articleLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    articleLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                            $ mconcat
                                [ const $ pure $ homeLink
                                , const $ pure $ searchBox
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleHeader prefix stats dloc
                                , assembleMain
                                , assembleGlossary
                                , assembleFooter
                                ]
                        ]
                ]

    spaLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    spaLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (div_ [id_ "spa", class_ "application"]) mempty
                        , wrap (div_ [class_ "help"])
                            $ wrap article_
                            $ mconcat
                                [ assembleMain
                                ]
                        ]
                ]

    imageGalleryLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    imageGalleryLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (div_ [id_ "gallery", class_ "photos"])
                            $ mconcat
                                [ assembleMain
                                ]
                        ]
                ]

    variousListingLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    variousListingLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (div_ [id_ "listing"])
                            $ mconcat
                                [ assembleMain
                                ]
                        ]
                ]

    topicsLayout ::
        TopicName ->
        [(Ext.Target a, Article [Text])] ->
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    topicsLayout topic articles dloc jsondloc txtdloc =
        let atomDLoc = destTopicAtom prefix topic
         in htmldoc
                $ mconcat
                    [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc atomDLoc) assembleStyle
                    , htmlbody
                        $ mconcat
                            [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                                $ mconcat
                                    [ const $ pure $ homeLink
                                    , const $ pure $ searchBox
                                    ]
                            , wrap (div_ [class_ "main"])
                                $ wrap article_
                                $ mconcat
                                    [ const (assembleTopicListing prefix stats topic articles)
                                    ]
                            ]
                    ]

    hashtagsLayout ::
        HashTagInfo ->
        [(Ext.Target a, Article [Text])] ->
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    hashtagsLayout tag articles dloc jsondloc txtdloc =
        let atomDLoc = destHashTagAtom prefix (hashtagValue tag)
         in htmldoc
                $ mconcat
                    [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc atomDLoc) assembleStyle
                    , htmlbody
                        $ mconcat
                            [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                                $ mconcat
                                    [ const $ pure $ homeLink
                                    , const $ pure $ searchBox
                                    ]
                            , wrap (div_ [class_ "main"])
                                $ wrap article_
                                $ mconcat
                                    [ const (assembleHashtagListing (hashtagValue tag) articles)
                                    ]
                            ]
                    ]

    glossaryListingLayout ::
        [(Ext.Target a, Article [Text])] ->
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    glossaryListingLayout articles dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                            $ mconcat
                                [ const $ pure $ homeLink
                                , const $ pure $ searchBox
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ const (assembleGlossaryListing prefix wholeGlossary articles)
                                ]
                        ]
                ]

    -- TODO: add warning on default layout
    defaultLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    defaultLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                            $ mconcat
                                [ const $ pure $ homeLink
                                , const $ pure $ searchBox
                                ]
                        , wrap (div_ [class_ "main"])
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
