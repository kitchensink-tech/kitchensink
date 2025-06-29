{-# LANGUAGE OverloadedRecordDot #-}

module KitchenSink.Layout.Blog.Fragments where

import Control.Monad (when)
import Data.Either (fromRight)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid as Lucid
import Lucid.Base qualified as Lucid
import Prelude ((+))

import Text.Atom.Feed qualified as Atom

import KitchenSink.Core.Assembler (runAssembler)
import KitchenSink.Core.Build.Site ()
import KitchenSink.Core.Build.Target (DestinationLocation, OutputPrefix, Url, destination, destinationUrl)
import KitchenSink.Core.Build.Target qualified as Core
import KitchenSink.Core.Section hiding (Section)
import KitchenSink.Prelude

import KitchenSink.Core.Assembler.Sections
import KitchenSink.Layout.Blog.Analyses
import KitchenSink.Layout.Blog.ArticleTypes
import KitchenSink.Layout.Blog.Destinations
import KitchenSink.Layout.Blog.Extensions (Article, Assembler, Section, Target)
import KitchenSink.Layout.Blog.Metadata

assembleHeader :: OutputPrefix -> TopicStats -> DestinationLocation -> Article [Text] -> Assembler (Lucid.Html ())
assembleHeader prefix stats currentDestination art =
    r
        <$> (extract <$> json @() @PreambleData art isPreamble)
        <*> (fmap extract <$> jsonm @() @SocialData art isSocial)
        <*> (fmap extract <$> jsonm @() @TopicData art isTopic)
  where
    r :: PreambleData -> Maybe SocialData -> Maybe TopicData -> Lucid.Html ()
    r content social topic = do
        let author_ = case social >>= twitter of
                Nothing -> span_ (toHtml . author $ content)
                Just handle -> a_ [href_ $ "https://twitter.com/" <> handle] (mconcat ["@", toHtml handle])

        let topiclist_ =
                case topic of
                    Nothing -> mempty
                    Just d ->
                        do
                            div_ [class_ "topiclist"]
                            $ mconcat
                                [ topicTag prefix stats currentDestination t | t <- topics d
                                ]

        let wc =
                toHtml
                    $ show
                    $ fromRight 0
                    $ contentWordCount
                    <$> runAssembler (getSections art isMainContent)
        let infos = analyzeArticle art
        let lc = toHtml . show . length $ infos.linkInfos
        let ic = toHtml . show . length $ infos.imageInfos
        let bc = toHtml . show . length $ infos.snippetInfos
        let headersentence pr =
                case preambleDateText pr of
                    Just txt ->
                        mconcat ["On ", span_ (toHtml txt), ", by ", author_, ", ", wc, " words, ", bc, " code snippets, ", lc, " links, ", ic, "images."]
                    Nothing ->
                        mconcat ["By ", author_, ", ", wc, " words, ", bc, " code snippets, ", lc, " links, ", ic, "images."]

        header_ [class_ "heading"] $ do
            h1_ (toHtml $ title $ content)
            p_ (headersentence content)
            topiclist_

assembleGlossary :: Article [Text] -> Assembler (Lucid.Html ())
assembleGlossary art = do
    gd <- fmap extract <$> jsonm @() @GlossaryData art isGlossary
    let out = maybe mempty r gd
    pure out
  where
    r :: GlossaryData -> Lucid.Html ()
    r gd =
        dl_ [class_ "glossary"] $ do
            mconcat $ fmap rterm (glossary gd)

    rterm :: GlossaryTerm -> Lucid.Html ()
    rterm (GlossaryTerm t d) = do
        dt_ [class_ "term"] (toHtml t)
        dd_ [class_ "definition"] (toHtml d)

assembleFooter :: Article [Text] -> Assembler (Lucid.Html ())
assembleFooter a = r <$> (fmap extract . jsonSection @SocialData =<< getSection a isSocial)
  where
    r :: SocialData -> Lucid.Html ()
    r s =
        footer_ [class_ "footing"] $ do
            div_ [class_ "social-links"] $ do
                maybe mempty twtr (twitter s)
                maybe mempty masto (mastodon s)
                maybe mempty chost (cohost s)
                maybe mempty ghub (github s)
                maybe mempty lkdn (linkedin s)

    twtr, ghub, lkdn, masto, chost :: Text -> Lucid.Html ()
    twtr h = a_ [href_ $ "https://twitter.com/" <> h] "twitter"
    ghub h = a_ [href_ $ "https://github.com/" <> h] "github"
    lkdn h = a_ [href_ $ "https://linkedin.com/in/" <> h] "linkedin"
    masto h = a_ [rel_ "me", href_ $ h] "mastodon"
    chost h = a_ [href_ $ "https://cohost.org/" <> h] "cohost"

assembleDefaultLayoutWarning :: Article [Text] -> Assembler (Lucid.Html ())
assembleDefaultLayoutWarning _ = pure $ do
    div_ [class_ "no-layout-notice"] $ do
        p_ "this page uses a default layout"

assembleUpcomingMain :: Article [Text] -> Assembler (Lucid.Html ())
assembleUpcomingMain a = r <$> (getSections a isMainContent >>= traverse renderSection)
  where
    r :: [Section PreRenderedHtml] -> Lucid.Html ()
    r content = do
        div_ [class_ "upcoming-notice"] $ do
            p_ "This article is still considered unfinished and content may change significantly."
        div_ [id_ "histogram"] mempty
        div_ [class_ "main-article"] $ do
            traverse_ f content

    f :: Section PreRenderedHtml -> Lucid.Html ()
    f content =
        section_ [class_ "main-section"] $ do
            toHtmlRaw @Text (extract' content)

assembleArchivedMain :: Article [Text] -> Assembler (Lucid.Html ())
assembleArchivedMain a = r <$> (getSections a isMainContent >>= traverse renderSection)
  where
    r :: [Section PreRenderedHtml] -> Lucid.Html ()
    r content = do
        div_ [class_ "archived-notice"] $ do
            p_ "This article is considered archived."
        div_ [id_ "histogram"] mempty
        div_ [class_ "main-article"] $ do
            traverse_ f content

    f :: Section PreRenderedHtml -> Lucid.Html ()
    f content =
        section_ [class_ "main-section"] $ do
            toHtmlRaw @Text (extract' content)

assembleMain :: Article [Text] -> Assembler (Lucid.Html ())
assembleMain a = r <$> (getSections a isMainContent >>= traverse renderSection)
  where
    r :: [Section PreRenderedHtml] -> Lucid.Html ()
    r content =
        div_ [class_ "main-article"] $ do
            traverse_ f content

    f :: Section PreRenderedHtml -> Lucid.Html ()
    f content =
        section_ [class_ "main-section"] $ do
            toHtmlRaw @Text (extract' content)

assembleStyle :: Article [Text] -> Assembler (Lucid.Html ())
assembleStyle a = r <$> (fmap Text.concat <$> getSection a isMainCss)
  where
    r :: Section Text -> Lucid.Html ()
    r content = style_ (extract content)

assembleTopicListing :: OutputPrefix -> TopicStats -> TopicName -> [(Target a, Article [Text])] -> Assembler (Lucid.Html ())
assembleTopicListing prefix stats topic articles =
    pure r
  where
    r :: Lucid.Html ()
    r = do
        header_ [class_ "heading"] $ do
            h1_ $ toHtml topic
        section_ [class_ "main"] $ do
            mainArticleLinks articles
        section_ [class_ "others"]
            $ mconcat
                [ h2_ [class_ "listing-callout"] "other topics"
                , div_ [class_ "topiclist"]
                    $ mconcat
                        [ topicListingTag prefix stats otherTopic | otherTopic <- Map.keys $ byTopic stats, otherTopic /= topic
                        ]
                ]

topicListingTag :: OutputPrefix -> TopicStats -> TopicName -> Lucid.Html ()
topicListingTag prefix stats topic =
    div_ [class_ "topic"] $ do
        a_ [class_ "topic-link", href_ url] $ do
            span_ [class_ "topic-name"] (toHtml topic)
            span_ [class_ "topic-count"] $ do
                toHtml $ show $ length articles
  where
    url = destinationUrl $ destTopic prefix topic
    articles = fromMaybe [] $ Map.lookup topic $ byTopic stats

type TagValue = Text.Text

assembleHashtagListing :: TagValue -> [(Target a, Article [Text])] -> Assembler (Lucid.Html ())
assembleHashtagListing tag articles =
    pure r
  where
    r :: Lucid.Html ()
    r = do
        header_ [class_ "heading"] $ do
            h1_ $ toHtml tag
        section_ [class_ "main"] $ do
            mainArticleLinks articles

assembleGlossaryListing :: OutputPrefix -> WholeGlossary -> [(Target a, Article [Text])] -> Assembler (Lucid.Html ())
assembleGlossaryListing _ g _ =
    pure r
  where
    r :: Lucid.Html ()
    r = do
        header_ [class_ "heading"] $ do
            h1_ $ toHtml ("Glossary" :: Text)
        section_ [class_ "main"] $ do
            mconcat [entry t arts | (t, arts) <- glossaryEntries g]

    entry :: Text -> [(Target (), Article [Text], Text)] -> Lucid.Html ()
    entry t defs = do
        div_ [class_ "entry"] $ do
            p_ $ toHtml t
            defWithLinks defs

    defWithLinks :: [(Target (), Article [Text], Text)] -> Lucid.Html ()
    defWithLinks defs =
        do
            ul_ [class_ "entry-details-list"]
            $ mconcat
                [ li_ [class_ "article-details-list-item"] $ defWithLink t art def
                | (t, art, def) <- defs
                ]

    defWithLink :: Target () -> Article [Text] -> Text -> Lucid.Html ()
    defWithLink t art def =
        div_ [class_ "deflink"] $ do
            p_ $ toHtml def
            span_ $ do
                toHtml ("In: " :: Text)
                articleLink t art
                toHtml ("." :: Text)

htmlbody ::
    (Article [Text] -> Assembler (Lucid.Html ())) ->
    (Article [Text] -> Assembler (Lucid.Html ()))
htmlbody = wrap body_

data MetaHeaders = MetaHeaders
    { extraMeta :: MetaData
    , destinationHTML :: DestinationLocation
    , destinationJSON :: DestinationLocation
    , destinationText :: DestinationLocation
    , destinationAtom :: DestinationLocation
    }

htmlhead ::
    MetaHeaders ->
    (Article [Text] -> Assembler (Lucid.Html ())) ->
    (Article [Text] -> Assembler (Lucid.Html ()))
htmlhead mh f = \art -> do
    hdrs1 <- metaheaders mh art
    hdrs2 <- f art
    hdrs3 <- (extraHeaders mh.extraMeta) art
    pure $ head_ (hdrs1 >> hdrs2 >> hdrs3)

-- see https://ogp.me/#types
-- see https://webcode.tools/generators/open-graph/article
-- see https://developer.twitter.com/en/docs/twitter-for-websites/cards/guides/getting-started
-- see https://www.linkedin.com/post-inspector/inspect/
metaheaders :: MetaHeaders -> Article [Text] -> Assembler (Lucid.Html ())
metaheaders mh art = do
    topic <- fmap extract <$> jsonm @() @TopicData art isTopic
    social <- fmap extract <$> jsonm @() @SocialData art isSocial
    summary <- fmap extract <$> lookupSection art isSummary
    preamble <- fmap extract <$> jsonm @() @PreambleData art isPreamble
    let titleTxt = maybe defaultTitle mktitle preamble :: Text
    let faviconHref = mkfavicon preamble :: Text
    pure
        $ mconcat
        $ catMaybes
            [ Just $ meta_ [charset_ "utf-8"]
            , Just $ meta_ [name_ "viewport", content_ "with=device-width, initial-scale=1.0"]
            , Just $ title_ $ toHtml titleTxt
            , Just $ link_ [rel_ "icon", type_ "image/x-icon", href_ faviconHref]
            , Just $ link_ [rel_ "alternate", type_ "application/atom+xml", title_ "Atom Feed", href_ urlForAtom]
            , fmap (\x -> meta_ [name_ "author", content_ $ author x]) preamble
            , fmap (\x -> meta_ [name_ "keywords", content_ $ Text.intercalate ", " $ topicKeywords x]) topic
            , fmap (\x -> meta_ [name_ "description", content_ $ compactSummary x]) summary
            , Just $ meta_ [name_ "twitter:card", content_ "summary"]
            , Just $ meta_ [name_ "twitter:title", content_ titleTxt]
            , (\x -> meta_ [name_ "twitter:site", content_ $ "@" <> x]) <$> twitterSiteLogin extra
            , (\x -> meta_ [name_ "twitter:creator", content_ $ "@" <> x]) <$> (twitter =<< social)
            , fmap (\x -> meta_ [property_ "twitter:description", content_ $ compactSummary x]) summary
            , Just $ meta_ [name_ "type", property_ "og:type", content_ "article"]
            , Just $ meta_ [name_ "title", property_ "og:title", content_ titleTxt]
            , Just $ meta_ [name_ "url", property_ "og:url", content_ $ urlForPage]
            , fmap (\x -> meta_ [property_ "og:image", content_ $ urlForImage $ x]) (topic >>= imageLink)
            , fmap (\x -> meta_ [property_ "twitter:image:src", content_ $ urlForImage $ x]) (topic >>= imageLink)
            , fmap (\x -> meta_ [property_ "og:description", content_ $ compactSummary x]) summary
            , fmap (\x -> meta_ [name_ "article:published_time", content_ $ fmtUTC x]) $ preamble >>= date
            , Just $ meta_ [name_ "ks:article_json", content_ $ urlForJSONPage]
            , Just $ meta_ [name_ "ks:article_text", content_ $ urlForTextPage]
            ]
        <> [meta_ [name_ "article:tag", content_ k] | k <- maybe [] topicKeywords topic]
  where
    extra = mh.extraMeta
    dloc = mh.destinationHTML
    jsondloc = mh.destinationJSON
    textdloc = mh.destinationText
    atomLoc = mh.destinationAtom
    fmtUTC = Text.pack . iso8601Show
    property_ = Lucid.makeAttribute "property"
    urlForPage = publishBaseURL extra <> destinationUrl dloc
    urlForAtom = publishBaseURL extra <> destinationUrl atomLoc
    urlForJSONPage = destinationUrl jsondloc
    urlForTextPage = destinationUrl textdloc
    urlForImage imgpath = publishBaseURL extra <> imgpath

    defaultTitle :: Text
    defaultTitle = baseTitle extra

    mktitle :: PreambleData -> Text
    mktitle x = mconcat [baseTitle extra, " - ", title x]

    mkfavicon :: Maybe PreambleData -> Text
    mkfavicon x = fromMaybe defaultFavicon (faviconUrl =<< x)

defaultFavicon :: Text
defaultFavicon = "/images/favicon.png"

wrap ::
    (Lucid.Html () -> Lucid.Html ()) ->
    (Article [Text] -> Assembler (Lucid.Html ())) ->
    (Article [Text] -> Assembler (Lucid.Html ()))
wrap k f = fmap k . f

htmldoc :: (Functor t) => (a -> t (Lucid.Html ())) -> a -> t LText.Text
htmldoc mkContent a =
    Lucid.renderText . doctypehtml_ <$> mkContent a

buildinfo :: Article [Text] -> Maybe BuildInfoData
buildinfo art =
    either (const Nothing) Just
        $ fmap extract
        $ runAssembler
        $ json @() @BuildInfoData art isBuildInfo

isPublishedArticle :: Article [Text] -> Bool
isPublishedArticle art = isPublic
  where
    isPublic :: Bool
    isPublic = case publicationStatus =<< buildinfo art of
        Just Upcoming -> False
        Just Archived -> False
        Just Public -> True
        Nothing -> True

isListableArticle :: Article [Text] -> Bool
isListableArticle art = not (layoutNameFor art `List.elem` [IndexPage, TopicListingTemplate, HashTagListingTemplate])

shouldShowStatsForArticle :: Article [Text] -> Bool
shouldShowStatsForArticle art =
    not (layoutNameFor art `List.elem` [IndexPage, HashTagListingTemplate, TopicListingTemplate, ImageGallery, VariousListing, SinglePageApp])

isConcreteTarget :: Article [Text] -> Bool
isConcreteTarget art = not (layoutNameFor art `List.elem` [HashTagListingTemplate, TopicListingTemplate, GlossaryPage])

compactTitle :: PreambleData -> Text
compactTitle p = mconcat [title p]

articleLink :: Target a -> Article [Text] -> Lucid.Html ()
articleLink (Core.Target d _ _) art =
    mylink_ url txt
  where
    url :: Text
    url = destinationUrl d

    txt :: Text
    txt =
        either (const url) (compactTitle . extract)
            $ runAssembler
            $ json @() @PreambleData art isPreamble

mylink_ :: Url -> Text -> Lucid.Html ()
mylink_ url txt = a_ [href_ url] (toHtml txt)

homeLink :: Lucid.Html ()
homeLink = mylink_ "/" "home"

searchBox :: Lucid.Html ()
searchBox = div_ [id_ "search-box"] $ do
    js_ "/js/search-box.js"

js_ :: Text -> Lucid.Html ()
js_ p = Lucid.termRawWith "script" [type_ "text/javascript", src_ p, async_ ""] ""

topicsListings :: TopicStats -> Lucid.Html ()
topicsListings stats =
    nav_ [class_ "topics-listing"] $ do
        h2_ [class_ "listing-callout"] "by topic"
        div_ [class_ "topics-listing-list"] $ do
            mconcat
                [ topicListing p
                | p <- Map.toList $ byTopic stats
                ]

topicListing :: (TopicName, [(Target a, Article [Text])]) -> Lucid.Html ()
topicListing (topic, tgts) =
    div_ [class_ "topics-listing-list-topic"] $ do
        strong_ [class_ "topic-name"] (toHtml topic)
        smallArticleLinks tgts

smallArticleLinks :: [(Target a, Article [Text])] -> Lucid.Html ()
smallArticleLinks tgts =
    ul_ [class_ "article-links-list"]
        $ mconcat
            [li_ [class_ "article-links-list-item"] $ uncurry articleLink t | t <- tgts]

topicTag :: OutputPrefix -> TopicStats -> DestinationLocation -> TopicName -> Lucid.Html ()
topicTag prefix stats currentDestination topic =
    div_ [class_ "topic"] $ do
        a_ [class_ "topic-link", href_ url] $ do
            span_ [class_ "topic-name"] (toHtml topic)
            span_ [class_ "topic-count"] $ do
                toHtml $ show $ 1 + length prevArticles
                toHtml ("/" :: Text)
                toHtml $ show $ length articles
        div_ [class_ "topic-prevnext"] $ do
            span_ [class_ "topic-prev-link"] $ do
                fromMaybe (pure ()) prevLink
            span_ [class_ "topic-next-link"] $ do
                fromMaybe (pure ()) nextLink
  where
    url = destinationUrl $ destTopic prefix topic
    articles = fromMaybe [] $ Map.lookup topic $ byTopic stats
    -- todo: extract the following into separate functions
    isOtherTarget (target2, _) = destination target2 /= currentDestination
    prevArticles = List.takeWhile isOtherTarget articles
    nextArticles = List.drop 1 $ List.dropWhile isOtherTarget articles
    previousArticle = if null prevArticles then Nothing else Just $ List.last prevArticles
    nextArticle = fst <$> List.uncons nextArticles

    tgtLink word (target, _) = mylink_ (destinationUrl $ destination target) word
    prevLink = fmap (tgtLink "[prev]") previousArticle
    nextLink = fmap (tgtLink "[next]") nextArticle

sortByDate :: [(a, Article [Text])] -> [(a, Article [Text])]
sortByDate = List.sortBy f
  where
    f (_, a1) (_, a2) = extractDate a2 `compare` extractDate a1

extractDate :: Article [Text] -> Maybe UTCTime
extractDate art =
    either (const Nothing) (date . extract)
        $ runAssembler
        $ json @() @PreambleData art isPreamble

mainArticleLinks :: [(Target a, Article [Text])] -> Lucid.Html ()
mainArticleLinks targets =
    articleListing "all articles"
        $ List.filter (isListableArticle . snd)
        $ sortByDate targets

latestArticleLink :: [(Target a, Article [Text])] -> Lucid.Html ()
latestArticleLink targets =
    articleListing "latest article"
        $ List.take 1
        $ List.filter (isPublishedArticle . snd)
        $ List.filter (isListableArticle . snd)
        $ sortByDate targets

articleListing :: Text -> [(Target a, Article [Text])] -> Lucid.Html ()
articleListing htext targets =
    nav_ [class_ "articles-listing"] $ do
        h2_ [class_ "listing-callout"] $ Lucid.toHtml htext
        div_ [class_ "articles-listing-list"] $ do
            mconcat
                [ uncurry mainArticleLinkWithAnnotation p
                | p <- targets
                ]

siteGraphEchartZone :: Lucid.Html ()
siteGraphEchartZone =
    nav_ [class_ "articles-graph"] $ do
        h2_ [class_ "listing-callout"] "site map"
        div_ [id_ "echartzone", style_ "width:800px;height:600px;"] mempty

mainArticleLinkWithAnnotation :: Target a -> Article [Text] -> Lucid.Html ()
mainArticleLinkWithAnnotation t art =
    div_ [class_ $ mconcat ["articles-listing-item", " ", publishStatusClass]] $ do
        span_ [class_ "article-date"] (toHtml formattedDate)
        div_ $ do
            statusIcon
            articleLink t art
            when (isPublishedArticle art) $ do
                articleSummary art
  where
    formattedDate :: Text
    formattedDate = fromMaybe "" $ preambleDateText =<< preamble

    publishStatusClass :: Text
    publishStatusClass = fromMaybe "" $ fmap f $ publicationStatus =<< buildinfo art
      where
        f Public = "article-status-published"
        f Upcoming = "article-status-upcoming"
        f Archived = "article-status-archived"

    statusIcon :: Lucid.Html ()
    statusIcon = case publicationStatus =<< buildinfo art of
        Nothing -> mempty
        Just Public -> mempty
        Just Upcoming -> span_ [class_ "notice-upcoming"] "(upcoming)"
        Just Archived -> span_ [class_ "notice-archived"] "(archived)"

    preamble :: Maybe PreambleData
    preamble =
        either (const Nothing) Just
            $ fmap extract
            $ runAssembler
            $ json @() @PreambleData art isPreamble

preambleDateText :: PreambleData -> Maybe Text
preambleDateText preamble =
    (datetxt preamble)
        <|> (fmtUTC <$> date preamble)
  where
    fmtUTC :: UTCTime -> Text
    fmtUTC = Text.pack . formatTime defaultTimeLocale "%a, %d %b %Y"

articleSummary :: Article [Text] -> Lucid.Html ()
articleSummary art = when (isJust x) $ do
    div_ [class_ "article-summary"] $ do
        when (shouldShowStatsForArticle art) $ do
            articleStats art
        articleImage art
        maybe mempty (toHtmlRaw @Text . extract') x
  where
    x :: Maybe (Section PreRenderedHtml)
    x = fromRight Nothing $ runAssembler (lookupSection art isSummary >>= traverse renderSection)

articleStats :: Article [Text] -> Lucid.Html ()
articleStats art = do
    div_ [class_ "article-stats"] $ do
        p_
            $ toHtml
            $ List.intercalate
                " "
                [ show wc
                , "words"
                , "/"
                , show . length . linkInfos $ infos
                , "links"
                , "/"
                , show . length . imageInfos $ infos
                , "images"
                , "/"
                , show . length . snippetInfos $ infos
                , "snippets"
                ]
  where
    infos = analyzeArticle art

    wc :: Int
    wc = contentWordCount xs

    xs :: [Section [Text]]
    xs = fromRight [] $ runAssembler (getSections art isMainContent)

articleImage :: Article [Text] -> Lucid.Html ()
articleImage art = do
    div_ [class_ "article-image"] $ do
        maybe mempty (\link -> img_ [src_ link]) imgUrl
  where
    imgUrl :: Maybe Text
    imgUrl = case runAssembler getLink of
        Left _ -> Nothing
        Right x -> x

    getLink :: Assembler (Maybe Text)
    getLink = f <$> jsonm @() @TopicData art isTopic

    f :: Maybe (Section TopicData) -> Maybe Text
    f sec = imageLink . extract =<< sec

assembleAtomEntry ::
    MetaData ->
    DestinationLocation ->
    Article [Text] ->
    Assembler (Atom.Entry)
assembleAtomEntry extra dloc art = do
    summary <- fmap (compactSummary . extract) <$> lookupSection art isSummary
    contents <- assembleMain art
    r
        <$> (extract <$> json @() @PreambleData art isPreamble)
        <*> pure summary
        <*> pure contents
  where
    r :: PreambleData -> Maybe Text -> Html () -> Atom.Entry
    r preamble summary htmlContents =
        let url = publishBaseURL extra <> destinationUrl dloc
            selfLink = (Atom.nullLink url){Atom.linkRel = Just $ Left "alternate"}
            base =
                Atom.nullEntry
                    (url)
                    (Atom.TextString preamble.title)
                    (fmtUTC $ fromMaybe epochUTCTime preamble.date)
         in base
                { Atom.entrySummary = fmap Atom.TextString summary
                , Atom.entryAuthors = [Atom.nullPerson{Atom.personName = author preamble}]
                , Atom.entryLinks = [selfLink]
                , Atom.entryContent = Just (Atom.HTMLContent $ LText.toStrict $ Lucid.renderText htmlContents)
                }

    fmtUTC = Text.pack . iso8601Show

assemblePreambleData :: Article [Text] -> Assembler (Maybe PreambleData)
assemblePreambleData art = do
    fmap extract <$> jsonm @() @PreambleData art isPreamble

articlePreambleData :: Article [Text] -> Maybe PreambleData
articlePreambleData =
    join . hush . runAssembler . assemblePreambleData

assembleTopicData :: Article [Text] -> Assembler (Maybe TopicData)
assembleTopicData art = do
    fmap extract <$> jsonm @() @TopicData art isTopic

articleTopicData :: Article [Text] -> Maybe TopicData
articleTopicData =
    join . hush . runAssembler . assembleTopicData

assembleGlossaryData :: Article [Text] -> Assembler (Maybe GlossaryData)
assembleGlossaryData art = do
    fmap extract <$> jsonm @() @GlossaryData art isGlossary

articleGlossaryData :: Article [Text] -> Maybe GlossaryData
articleGlossaryData =
    join . hush . runAssembler . assembleGlossaryData

compactSummary :: [Text] -> Text
compactSummary = Text.strip . Text.intercalate " "

assembleCompactSummary :: Article [Text] -> Assembler Text
assembleCompactSummary art = do
    summary <- fmap extract <$> lookupSection art isSummary
    pure $ maybe "" compactSummary summary

assembleTitle :: Article [Text] -> Assembler (Maybe Text)
assembleTitle art = do
    preamble <- fmap extract <$> jsonm @() @PreambleData art isPreamble
    pure (title <$> preamble)

articleCompactSummary :: Article [Text] -> Maybe Text
articleCompactSummary =
    hush . runAssembler . assembleCompactSummary

articleTitle :: Article [Text] -> Maybe Text
articleTitle =
    join . hush . runAssembler . assembleTitle
