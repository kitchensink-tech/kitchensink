{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KitchenSink.Layout.Blog.Targets (
    siteTargets,
    siteDiagnostics,
    PathList,
    TargetType,
    PreambleSummary,
    TargetSummary,
    TopicSummary,
    GlossarySummary,
) where

import Data.Aeson (ToJSON, encode)
import Data.Foldable (concatMap)
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
import KitchenSink.Layout.Base (Diagnostic (..), Severity (..))
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

imageTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
imageTargets urlPrefix prefix site =
    [simpleTarget ImageTarget (destImage urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.images]

dotimageTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
dotimageTargets urlPrefix prefix site =
    [ simpleTarget
        GraphVizImageTarget
        (destGenImage urlPrefix prefix loc GenPngFile)
        (execCmd root "dot" ["-Tpng", "-o", "/dev/stdout", path] "")
    | Sourced loc@(FileSource path) _ <- site.dotSourceFiles
    ]
  where
    root :: ExecRoot
    root = Nothing

videoTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
videoTargets urlPrefix prefix site =
    [simpleTarget VideoTarget (destVideoFile urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.videoFiles]

audioTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
audioTargets urlPrefix prefix site =
    [simpleTarget AudioTarget (destAudioFile urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.audioFiles]

rawTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
rawTargets urlPrefix prefix site =
    [simpleTarget RawTarget (destRawFile urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.rawFiles]

documentTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
documentTargets urlPrefix prefix site =
    [simpleTarget DocumentTarget (destDocumentFile urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.docFiles]

cssTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
cssTargets urlPrefix prefix site =
    [simpleTarget CssTarget (destCssFile urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.cssFiles]

webfontTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
webfontTargets urlPrefix prefix site =
    [simpleTarget WebfontTarget (destWebfontFile urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.webfontFiles]

jsTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
jsTargets urlPrefix prefix site =
    [simpleTarget JavaScriptSourceTarget (destJsFile urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.jsFiles]

htmlTargets :: UrlPrefix -> OutputPrefix -> Site -> [Target]
htmlTargets urlPrefix prefix site =
    [simpleTarget HtmlSourceTarget (destHtml urlPrefix prefix loc) (copyFrom loc) | Sourced loc _ <- site.htmlFiles]

jsonDataTarget :: (ToJSON a) => UrlPrefix -> OutputPrefix -> a -> FilePath -> Target
jsonDataTarget urlPrefix prefix v loc =
    simpleTarget JSONTarget (destJsonDataFile urlPrefix prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ LByteString.toStrict $ encode v

textDataTarget :: UrlPrefix -> OutputPrefix -> Article [Text] -> FilePath -> Target
textDataTarget urlPrefix prefix v loc =
    simpleTarget JSONTarget (destTextDataFile urlPrefix prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ Text.encodeUtf8 $ TextRender.textRender v

rootDataTarget :: UrlPrefix -> OutputPrefix -> Text -> FilePath -> Target
rootDataTarget urlPrefix prefix v loc =
    simpleTarget RootFileTarget (destRootDataFile urlPrefix prefix loc) (Core.ProduceGenerator f)
  where
    f _ = Generator $ pure $ Right $ Text.encodeUtf8 v

generatorInstructions :: Article [Text] -> Assembler [GeneratorInstructionsData]
generatorInstructions art =
    getSections art isGeneratorInstructions
        >>= traverse (fmap extract . jsonSection)

-- | What 'siteTargets' silently works around: an article whose @layout@ is not
-- one we know (or whose build-info section cannot be read) is rendered with the
-- default layout, and an article whose generator sections cannot be read gets no
-- generator targets.
siteDiagnostics :: Site -> [Diagnostic]
siteDiagnostics site = concatMap articleDiagnostics site.articles
  where
    articleDiagnostics :: Sourced (Article [Text]) -> [Diagnostic]
    articleDiagnostics (Sourced loc art) =
        layoutDiagnostics loc art <> generatorDiagnostics loc art

    layoutDiagnostics :: SourceLocation -> Article [Text] -> [Diagnostic]
    layoutDiagnostics loc art
        | not (isConcreteTarget art) = []
        | otherwise = case layoutNameFor art of
            UnknownLayout name ->
                [Diagnostic Warning loc ("unknown layout " <> Text.pack (show name) <> " (with this publicationStatus), rendered with the default layout")]
            ErrorLayout err ->
                [Diagnostic Warning loc ("unreadable build-info section (" <> Text.pack (show err) <> "), rendered with the default layout")]
            _ -> []

    generatorDiagnostics :: SourceLocation -> Article [Text] -> [Diagnostic]
    generatorDiagnostics loc art =
        either
            (\err -> [Diagnostic Failure loc ("unreadable generator section (" <> Text.pack (show err) <> "), no generator target produced")])
            (const [])
            $ runAssembler
            $ generatorInstructions art

siteTargets :: ExecRoot -> OutputPrefix -> MetaData -> Site -> [Target]
siteTargets execRoot prefix extra site = allTargets
  where
    urlPrefix :: UrlPrefix
    urlPrefix = extra.pathPrefix

    allTargets =
        mconcat
            [ embeddedGeneratorTargets
            , embeddedDataTargets
            , fmap fst articleTargets
            , imageTargets urlPrefix prefix site
            , dotimageTargets urlPrefix prefix site
            , videoTargets urlPrefix prefix site
            , audioTargets urlPrefix prefix site
            , rawTargets urlPrefix prefix site
            , documentTargets urlPrefix prefix site
            , cssTargets urlPrefix prefix site
            , webfontTargets urlPrefix prefix site
            , jsTargets urlPrefix prefix site
            , htmlTargets urlPrefix prefix site
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
        [ jsonDataTarget urlPrefix prefix (pathList) "paths.json"
        , jsonDataTarget urlPrefix prefix (filecounts site) "filecounts.json"
        , jsonDataTarget urlPrefix prefix (topicsgraph urlPrefix (ExternalSitesInfo $ externalKitchenSinkURLs extra) stats) "topicsgraph.json"
        ]
            <> [ jsonDataTarget urlPrefix prefix (analyzeArticle art) (p <> ".json") | (Sourced (FileSource p) art) <- site.articles
               ]

    textDataTargets :: [Target]
    textDataTargets =
        [ textDataTarget urlPrefix prefix art (p <> ".text") | (Sourced (FileSource p) art) <- site.articles
        ]

    seoTargets :: [Target]
    seoTargets =
        [ rootDataTarget urlPrefix prefix (Text.unlines $ fmap (\x -> publishBaseURL extra <> x) $ fmap (destinationUrl . destination . fst) articleTargets) "sitemap.txt"
        , rootDataTarget urlPrefix prefix (atomFeedContent articleTargets) "atom.xml"
        ]

    atomFeedContent :: [(Ext.Target z, Article [Text])] -> Text
    atomFeedContent targets =
        let render = LText.toStrict . fromJust . Export.textFeedWith def . AtomFeed
            uri = publishBaseURL extra <> (destinationUrl $ destRootDataFile urlPrefix prefix "atom.xml")
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
        let u = destHtml urlPrefix prefix loc
            j = destJsonDataFile urlPrefix prefix (path <> ".json") -- todo:unify
            t = destTextDataFile urlPrefix prefix (path <> ".text") -- todo:unify
            tgtSummary = TargetSummary ArticleTarget (articleTitle art) (articleCompactSummary art) (summarizePreamble <$> articlePreambleData art) (summarizeTopic <$> articleTopicData art) (summarizeGlossary <$> articleGlossaryData art) (summarizeHashTags $ analyzeArticle art)
         in target tgtSummary u (Core.ProduceAssembler $ layoutFor u j t art)

    articleTargets :: [(Target, Article [Text])]
    articleTargets =
        [ (articleTarget srca, srca.obj)
        | srca <- site.articles
        , isConcreteTarget srca.obj
        ]

    -- a section that cannot be read yields no target here; 'siteDiagnostics' reports it
    embeddedGeneratorTargets :: [Target]
    embeddedGeneratorTargets =
        [ tgt
        | Sourced loc art <- site.articles
        , tgt <- getTargets loc art
        ]
      where
        getTargets :: SourceLocation -> Article [Text] -> [Target]
        getTargets loc art =
            either (const []) (fmap (generatorTarget loc))
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
             in simpleTarget GeneratedTarget (destGenArbitrary urlPrefix prefix loc g) rule

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
                dataDestination = destEmbeddedData urlPrefix prefix loc (destinationExtension format) name index
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
        [ let u = destTopic urlPrefix prefix topic
           in simpleTarget TopicsIndexTarget u (Core.ProduceAssembler $ topicsLayout topic articles u u u art)
        | (topic, articles) <- Map.toList (byTopic stats)
        ]

    topicAtomTargets :: Maybe (Article [Text]) -> [Target]
    topicAtomTargets Nothing = []
    topicAtomTargets (Just _) =
        [ let u = destTopicAtom urlPrefix prefix topic
              rule = Core.ProduceAssembler $ pure $ LText.fromStrict $ atomFeedContent articles
           in simpleTarget TopicsIndexTarget u rule
        | (topic, articles) <- Map.toList (byTopic stats)
        ]

    hashtagIndexesTargets :: Maybe (Article [Text]) -> [Target]
    hashtagIndexesTargets Nothing = []
    hashtagIndexesTargets (Just art) =
        [ let u = destHashTag urlPrefix prefix (hashtagValue tag)
           in simpleTarget HashTagsIndexTarget u (Core.ProduceAssembler $ hashtagsLayout tag articles u u u art)
        | (tag, articles) <- Map.toList (byHashTag stats)
        ]

    hashtagAtomTargets :: Maybe (Article [Text]) -> [Target]
    hashtagAtomTargets Nothing = []
    hashtagAtomTargets (Just _) =
        [ let u = destHashTagAtom urlPrefix prefix (hashtagValue tag)
              rule = Core.ProduceAssembler $ pure $ LText.fromStrict $ atomFeedContent articles
           in simpleTarget HashTagsIndexTarget u rule
        | (tag, articles) <- Map.toList (byHashTag stats)
        ]

    glossaryTargets :: Maybe (Sourced (Article [Text])) -> [Target]
    glossaryTargets Nothing = []
    glossaryTargets (Just (Sourced loc art)) =
        let u = destHtml urlPrefix prefix loc
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
        , (DocumentationPage, documentationLayout)
        , (IndexPage, indexLayout)
        ]

    stats :: TopicStats
    stats = buildTopicStats site.articles (fmap (const ()) . articleTarget)

    wholeGlossary :: WholeGlossary
    wholeGlossary = buildWholeGlossary site.articles (fmap (const ()) . articleTarget)

    rootAtomDLoc :: DestinationLocation
    rootAtomDLoc = destRootDataFile urlPrefix prefix "atom.xml"

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
                        [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                            $ mconcat
                                [ const $ pure $ homeLink extra
                                , const $ pure $ searchBox urlPrefix
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleMain urlPrefix
                                , const $ pure $ latestArticleLink urlPrefix articleTargets
                                , const $ pure $ siteGraphEchartZone urlPrefix
                                , const $ pure $ mainArticleLinks urlPrefix articleTargets
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
                                [ const $ pure $ homeLink extra
                                , const $ pure $ searchBox urlPrefix
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleHeader urlPrefix prefix stats dloc
                                , assembleArchivedMain urlPrefix
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
                                [ const $ pure $ homeLink extra
                                , const $ pure $ searchBox urlPrefix
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleHeader urlPrefix prefix stats dloc
                                , assembleUpcomingMain urlPrefix
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
                                [ const $ pure $ homeLink extra
                                , const $ pure $ searchBox urlPrefix
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleHeader urlPrefix prefix stats dloc
                                , assembleMain urlPrefix
                                , assembleGlossary
                                , assembleFooter
                                ]
                        ]
                ]

    documentationLayout ::
        DestinationLocation ->
        DestinationLocation ->
        DestinationLocation ->
        Article [Text] ->
        Assembler LText.Text
    documentationLayout dloc jsondloc txtdloc =
        htmldoc
            $ mconcat
                [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc rootAtomDLoc) assembleStyle
                , htmlbody
                    $ mconcat
                        [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                            $ mconcat
                                [ const $ pure $ homeLink extra
                                , const $ pure $ searchBox urlPrefix
                                ]
                        , wrap (div_ [class_ "main doc-layout"])
                            $ mconcat
                                [ assembleDocumentationToc
                                , wrap article_
                                    $ mconcat
                                        [ assembleHeader urlPrefix prefix stats dloc
                                        , assembleMain urlPrefix
                                        , assembleGlossary
                                        , const $ pure $ documentationPager articleTargets dloc
                                        , assembleFooter
                                        ]
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
                                [ assembleMain urlPrefix
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
                                [ assembleMain urlPrefix
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
                                [ assembleMain urlPrefix
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
        let atomDLoc = destTopicAtom urlPrefix prefix topic
         in htmldoc
                $ mconcat
                    [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc atomDLoc) assembleStyle
                    , htmlbody
                        $ mconcat
                            [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                                $ mconcat
                                    [ const $ pure $ homeLink extra
                                    , const $ pure $ searchBox urlPrefix
                                    ]
                            , wrap (div_ [class_ "main"])
                                $ wrap article_
                                $ mconcat
                                    [ const (assembleTopicListing urlPrefix prefix stats topic articles)
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
        let atomDLoc = destHashTagAtom urlPrefix prefix (hashtagValue tag)
         in htmldoc
                $ mconcat
                    [ htmlhead (MetaHeaders extra dloc jsondloc txtdloc atomDLoc) assembleStyle
                    , htmlbody
                        $ mconcat
                            [ wrap (nav_ [id_ "site-navigation", class_ "nav"])
                                $ mconcat
                                    [ const $ pure $ homeLink extra
                                    , const $ pure $ searchBox urlPrefix
                                    ]
                            , wrap (div_ [class_ "main"])
                                $ wrap article_
                                $ mconcat
                                    [ const (assembleHashtagListing urlPrefix (hashtagValue tag) articles)
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
                                [ const $ pure $ homeLink extra
                                , const $ pure $ searchBox urlPrefix
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ const (assembleGlossaryListing prefix wholeGlossary articles)
                                ]
                        ]
                ]

    -- see 'siteDiagnostics' for the build-time report of the fallback
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
                                [ const $ pure $ homeLink extra
                                , const $ pure $ searchBox urlPrefix
                                ]
                        , wrap (div_ [class_ "main"])
                            $ wrap article_
                            $ mconcat
                                [ assembleHeader urlPrefix prefix stats dloc
                                , assembleDefaultLayoutWarning
                                , assembleMain urlPrefix
                                , assembleGlossary
                                , assembleFooter
                                ]
                        ]
                ]
