module KitchenSink.Blog.Layout where

import Prelude ((+))
import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Lucid as Lucid
import qualified Lucid.Base as Lucid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.List as List
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.FilePath.Posix ((</>), takeFileName, takeBaseName)

-- import Text.Feed.Types (Feed(AtomFeed))
-- import Text.XML (def, rsPretty)
import qualified Text.Atom.Feed as Atom
-- import qualified Text.Feed.Export as Export (textFeedWith)

import KitchenSink.Blog.Target
import KitchenSink.Blog.Section
import KitchenSink.Blog.Site
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Basics

import KitchenSink.Blog.AssembleSections
import KitchenSink.Blog.Wordcount
import KitchenSink.Blog.Destinations
import KitchenSink.Blog.Advanced

assembleHeader :: OutputPrefix -> TopicStats -> DestinationLocation -> Article [Text] -> Assembler (Lucid.Html ())
assembleHeader prefix stats currentDestination art =
    r <$> (extract <$> json @PreambleData art Preamble)
      <*> (fmap extract <$> jsonm @SocialData art Social)
      <*> (fmap extract <$> jsonm @TopicData art Topic)
  where
    r :: PreambleData -> Maybe SocialData -> Maybe TopicData -> Lucid.Html ()
    r content social topic = do
      let author_ = case social of
                      Nothing -> span_ (toHtml . author $ content)
                      Just d -> a_ [ href_ $ "https://twitter.com/" <> twitter d ] (mconcat ["@", toHtml $ twitter d])

      let taglist_ =
            case topic of
              Nothing -> mempty
              Just d -> do
                div_ [ class_ "taglist" ]
                $ mconcat
                  [ topicTag prefix stats currentDestination tag | tag <- tags d
                  ]
   
      let wc = toHtml
               $ show
               $ fromRight 0
               $ contentWordCount
               <$> runAssembler (getSections art MainContent)
      let infos = analyzeArticle art
      let lc = toHtml . show . length . linkInfos $ infos
      let ic = toHtml . show . length . imageInfos $ infos
      let bc = toHtml . show . length . snippetInfos $ infos
      let headersentence pr =
            case preambleDateText pr of
              Just txt ->
                mconcat ["On ", span_ (toHtml txt), ", by ", author_, ", ", wc, " words, ", bc, " code snippets, ", lc, " links, ", ic, "images."]
              Nothing ->
                mconcat ["By ", author_, ", ", wc, " words, ", bc, " code snippets, ",  lc, " links, ", ic, "images."]

      header_ [ class_ "heading" ] $ do
        h1_ (toHtml $ title $ content)
        p_ (headersentence content)
        taglist_


assembleFooter :: Article [Text] -> Assembler (Lucid.Html ())
assembleFooter a = r <$> (fmap extract . jsonSection @SocialData =<< getSection a Social)
  where
    r :: SocialData -> Lucid.Html ()
    r s =
      footer_ [ class_ "footing" ] $ do
        div_ [ class_ "social-links" ] $ do
          twtr $ twitter s 
          maybe mempty masto (mastodon s)
          maybe mempty chost (cohost s)
          maybe mempty ghub (github s)
          maybe mempty lkdn (linkedin s)

    twtr, ghub, lkdn, masto, chost :: Text -> Lucid.Html ()
    twtr h = a_ [ href_ $ "https://twitter.com/" <> h ] (mconcat ["twitter: @", toHtml h])
    ghub h = a_ [ href_ $ "https://github.com/" <> h ] (mconcat ["github: ", toHtml $ h])
    lkdn h = a_ [ href_ $ "https://linkedin.com/in/" <> h ] (mconcat ["linkedin: ", toHtml $ h])
    masto h = a_ [ href_ $ h ] (mconcat ["mastodon: ", toHtml $ h])
    chost h = a_ [ href_ $ "https://cohost.org/" <> h ] (mconcat ["cohost: @", toHtml h])

assembleDefaultLayoutWarning :: Article [Text] -> Assembler (Lucid.Html ())
assembleDefaultLayoutWarning _ = pure $ do
  div_ [ class_ "no-layout-notice" ] $ do
    p_ "this page uses a default layout"

assembleUpcomingMain :: Article [Text] -> Assembler (Lucid.Html ())
assembleUpcomingMain a = r <$> (getSections a MainContent >>= traverse renderSection)
  where
    r :: [Section PreRenderedHtml] -> Lucid.Html ()
    r content = do
      div_ [ class_ "upcoming-notice" ] $ do
        p_ "This article is still considered unfinished and content may change significantly."
      div_ [ id_ "histogram" ] mempty
      div_ [ class_ "main-article" ] $ do
        traverse_ f content

    f :: Section PreRenderedHtml -> Lucid.Html ()
    f content =
      section_ [ class_ "main-section" ] $ do
        toHtmlRaw @Text (extract' content)

assembleMain :: Article [Text] -> Assembler (Lucid.Html ())
assembleMain a = r <$> (getSections a MainContent >>= traverse renderSection)
  where
    r :: [Section PreRenderedHtml] -> Lucid.Html ()
    r content =
      div_ [ class_ "main-article" ] $ do
        traverse_ f content

    f :: Section PreRenderedHtml -> Lucid.Html ()
    f content =
      section_ [ class_ "main-section" ] $ do
        toHtmlRaw @Text (extract' content)

assembleStyle :: Article [Text] -> Assembler (Lucid.Html ())
assembleStyle a = r <$> (fmap Text.concat <$> getSection a MainCss)
  where
    r :: Section Text -> Lucid.Html ()
    r content = style_ (extract content)

assembleTopicListing :: OutputPrefix -> TopicStats -> Tag -> Assembler (Lucid.Html ())
assembleTopicListing prefix stats tag =
    pure r
  where
    articles :: [ (Target (), Article [Text]) ]
    articles = fromMaybe [] $ Map.lookup tag (byTopic stats)

    r :: Lucid.Html ()
    r = do
      header_ [ class_ "heading" ] $ do
        h1_ $ toHtml tag
      section_ [ class_ "main" ] $ do
        mainArticleLinks articles
      section_ [ class_ "others" ]
        $ mconcat
          [ h2_ [ class_ "listing-callout" ] "other topics"
          , div_ [ class_ "taglist" ]
            $ mconcat
              [ topicListingTag prefix stats otherTag | otherTag <- Map.keys $ byTopic stats , otherTag /= tag
              ]
          ]

topicListingTag :: OutputPrefix -> TopicStats -> Tag -> Lucid.Html ()
topicListingTag prefix stats tag =
  div_ [ class_ "tag" ] $ do
    a_ [ class_ "tag-link", href_ url ] $ do
      span_ [ class_ "tag-name" ] (toHtml tag)
      span_ [ class_ "tag-count" ] $ do
        toHtml $ show $ length articles
  where
    url = destinationUrl $ destTag prefix tag
    articles = fromMaybe [] $ Map.lookup tag $ byTopic stats


htmlbody
  :: (Article [Text] -> Assembler (Lucid.Html ()))
  -> (Article [Text] -> Assembler (Lucid.Html ()))
htmlbody = wrap body_

htmlhead
  :: MetaExtraData
  -> DestinationLocation
  -> DestinationLocation
  -> (Article [Text] -> Assembler (Lucid.Html ()))
  -> (Article [Text] -> Assembler (Lucid.Html ()))
htmlhead extra dloc jsondloc f = \art -> do
  hdrs1 <- metaheaders extra dloc jsondloc art
  hdrs2 <- f art
  hdrs3 <- (extraHeaders extra) art
  pure $ head_ (hdrs1 >> hdrs2 >> hdrs3)


data MetaExtraData = MetaExtraData
  { now :: UTCTime
  , baseTitle :: Text
  , publishBaseURL :: Text
  , twitterSiteLogin :: Maybe Text
  , extraHeaders :: Article [Text] -> Assembler (Lucid.Html ())
  }

-- see https://ogp.me/#types
-- see https://webcode.tools/generators/open-graph/article
-- see https://developer.twitter.com/en/docs/twitter-for-websites/cards/guides/getting-started
-- see https://www.linkedin.com/post-inspector/inspect/
metaheaders :: MetaExtraData -> DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler (Lucid.Html ())
metaheaders extra dloc jsondloc art = do
  topic <- fmap extract <$> jsonm @TopicData art Topic
  social <- fmap extract <$> jsonm @SocialData art Social
  summary <- fmap extract <$> lookupSection art Summary
  preamble <- fmap extract <$> jsonm @PreambleData art Preamble
  let titleTxt = maybe defaultTitle mktitle preamble :: Text
  let faviconHref = mkfavicon preamble :: Text
  pure
    $ mconcat
    $ catMaybes
    [ Just $ meta_ [ charset_ "utf-8" ]
    , Just $ meta_ [ name_ "viewport" , content_ "with=device-width, initial-scale=1.0" ]
    , Just $ title_ $ toHtml titleTxt
    , Just $ link_ [ rel_ "icon" , type_ "image/x-icon", href_ faviconHref ]
    , Just $ link_ [ rel_ "alternate" , type_ "application/atom+xml", title_ "Atom Feed", href_ "/atom.xml" ]
    , fmap (\x -> meta_ [ name_ "author" , content_ $ author x]) preamble
    , fmap (\x -> meta_ [ name_ "keywords" , content_ $ Text.intercalate ", " $ topicKeywords x ]) topic
    , fmap (\x -> meta_ [ name_ "description" , content_ $ compactSummary x ]) summary

    , Just $ meta_ [ name_ "twitter:card" , content_ "summary"]
    , Just $ meta_ [ name_ "twitter:title" , content_ titleTxt ]
    , (\x -> meta_ [ name_ "twitter:site" , content_ $ "@" <> x ]) <$> twitterSiteLogin extra
    , fmap (\x -> meta_ [ name_ "twitter:creator" , content_ $ "@" <> twitter x]) social
    , fmap (\x -> meta_ [ property_ "twitter:description" , content_ $ compactSummary x ]) summary

    , Just $ meta_ [ name_ "type" , property_ "og:type", content_ "article" ]
    , Just $ meta_ [ name_ "title" , property_ "og:title", content_ titleTxt ]
    , Just $ meta_ [ name_ "url" , property_ "og:url", content_ $ urlForPage ]
    , fmap (\x -> meta_ [ property_ "og:image", content_ $ urlForImage $ x ]) (topic >>= imageLink)
    , fmap (\x -> meta_ [ property_ "twitter:image:src", content_ $ urlForImage $ x ]) (topic >>= imageLink)
    , fmap (\x -> meta_ [ property_ "og:description" , content_ $ compactSummary x ]) summary
    , fmap (\x -> meta_ [ name_ "article:published_time" , content_ $ fmtUTC x]) $ preamble >>= date
    , Just $ meta_ [ name_ "ks:article_json" , content_ $ urlForJSONPage ]
    ] <> [ meta_ [ name_ "article:tag", content_ k ]  | k <- maybe [] topicKeywords topic ]
  where
    fmtUTC = Text.pack . iso8601Show
    property_ = Lucid.makeAttribute "property"
    urlForPage = publishBaseURL extra <> destinationUrl dloc
    urlForJSONPage = destinationUrl jsondloc
    urlForImage imgpath = publishBaseURL extra <> imgpath

    defaultTitle :: Text
    defaultTitle = baseTitle extra

    mktitle :: PreambleData -> Text
    mktitle x = mconcat [ baseTitle extra, " - ", title x ]

    mkfavicon :: Maybe PreambleData -> Text
    mkfavicon x = fromMaybe defaultFavicon (faviconUrl =<< x)

defaultFavicon :: Text
defaultFavicon = "/images/favicon.png"

wrap :: (Lucid.Html () -> Lucid.Html ())
  -> (Article [Text] -> Assembler (Lucid.Html ()))
  -> (Article [Text] -> Assembler (Lucid.Html ()))
wrap k f = fmap k . f

htmldoc :: Functor t => (a -> t (Lucid.Html ())) -> a -> t LText.Text
htmldoc mkContent a =
     Lucid.renderText . doctypehtml_<$> mkContent a

data ArticleLayout
 = UnknownLayout Text
 | ErrorLayout AssemblerError
 | PublishedArticle
 | UpcomingArticle
 | IndexPage
 | TopicListingPage
 | SinglePageApp
 | ImageGallery
 | VariousListing
 deriving (Show, Eq)

layoutNameFor :: Article [Text] -> ArticleLayout
layoutNameFor art = 
  case runAssembler (json @BuildInfoData art BuildInfo) of
     Left err -> ErrorLayout err 
     Right s -> let binfo = extract s
                in case publicationStatus binfo of
                     Nothing     -> effectiveLayout Public (layout binfo)
                     Just status -> effectiveLayout status (layout binfo)

effectiveLayout :: PublicationStatus -> Text -> ArticleLayout
effectiveLayout Public "article" = PublishedArticle
effectiveLayout Public "index" = IndexPage
effectiveLayout Public "tags" = TopicListingPage
effectiveLayout Public "application" = SinglePageApp
effectiveLayout Public "gallery" = ImageGallery
effectiveLayout Public "listing" = VariousListing
effectiveLayout Public t = UnknownLayout t
effectiveLayout Upcoming "article" = UpcomingArticle
effectiveLayout Upcoming "application" = SinglePageApp
effectiveLayout Upcoming "gallery" = ImageGallery
effectiveLayout Upcoming "listing" = VariousListing
effectiveLayout Upcoming t = UnknownLayout t

isPublishedArticle :: Article [Text] -> Bool
isPublishedArticle art = isPublic
  where
    isPublic :: Bool
    isPublic = case publicationStatus =<< buildinfo art of
      Just Upcoming -> False
      Just Public -> True
      Nothing -> True

buildinfo :: Article [Text] -> Maybe BuildInfoData
buildinfo art =
  either (const Nothing) Just
  $ fmap extract
  $ runAssembler
  $ json @BuildInfoData art BuildInfo

isListableArticle :: Article [Text] -> Bool
isListableArticle art = not (layoutNameFor art `List.elem` [IndexPage, TopicListingPage])

shouldShowStatsForArticle :: Article [Text] -> Bool
shouldShowStatsForArticle art =
  not (layoutNameFor art `List.elem` [IndexPage, TopicListingPage, ImageGallery, VariousListing, SinglePageApp])

isConcreteTarget :: Article [Text] -> Bool
isConcreteTarget art = not (layoutNameFor art `List.elem` [TopicListingPage])

compactTitle :: PreambleData -> Text
compactTitle p = mconcat [ title p ]

articleLink :: Target a -> Article [Text] -> Lucid.Html ()
articleLink (Target d _ _) art =
    mylink_ url txt
  where
    url :: Text
    url = destinationUrl d

    txt :: Text
    txt = either (const url) (compactTitle . extract)
          $ runAssembler
          $ json @PreambleData art Preamble

mylink_ :: Url -> Text -> Lucid.Html ()
mylink_ url txt = a_ [ href_  url ] (toHtml txt)

homeLink :: Lucid.Html ()
homeLink = mylink_ "/" "home"

searchBox :: Lucid.Html ()
searchBox = div_ [ id_ "search-box" ] $ do
    js_ "/js/search-box.js"

js_ :: Text -> Lucid.Html ()
js_ p = Lucid.termRawWith "script" [ type_ "text/javascript" , src_ p , async_ "" ] ""

topicsListings :: TopicStats -> Lucid.Html ()
topicsListings stats =
  nav_ [ class_ "topics-listing" ] $ do
    h2_ [ class_ "listing-callout"] "by topic"
    div_ [ class_ "topics-listing-list" ] $ do
      mconcat
        [ topicListing p
        | p <- Map.toList $ byTopic stats
        ]

topicListing :: (Tag, [(Target a,Article [Text])]) -> Lucid.Html ()
topicListing (tag,tgts) =
  div_ [ class_ "topics-listing-list-topic" ] $ do
    strong_ [ class_ "tag-name" ] (toHtml tag)
    smallArticleLinks tgts

smallArticleLinks :: [(Target a,Article [Text])] -> Lucid.Html ()
smallArticleLinks tgts =
  ul_ [ class_ "article-links-list" ]
  $ mconcat
  [ li_ [ class_ "article-links-list-item" ] $ uncurry articleLink t | t <- tgts ]

topicTag :: OutputPrefix ->  TopicStats -> DestinationLocation -> Tag -> Lucid.Html ()
topicTag prefix stats currentDestination tag =
  div_ [ class_ "tag" ] $ do
    a_ [ class_ "tag-link", href_ url ] $ do
      span_ [ class_ "tag-name" ] (toHtml tag)
      span_ [ class_ "tag-count" ] $ do
        toHtml $ show $ 1 + length prevArticles
        toHtml ("/" :: Text)
        toHtml $ show $ length articles
    div_ [ class_ "tag-prevnext" ] $ do
      span_ [ class_ "tag-prev-link" ] $ do
        fromMaybe (pure ()) prevLink
      span_ [ class_ "tag-next-link" ] $ do
        fromMaybe (pure ()) nextLink
  where
    url = destinationUrl $ destTag prefix tag
    articles = fromMaybe [] $ Map.lookup tag $ byTopic stats
    -- todo: extract the following into separate functions
    isOtherTarget (target2, _) = destination target2 /= currentDestination
    prevArticles = List.takeWhile isOtherTarget articles
    nextArticles = List.drop 1 $ List.dropWhile isOtherTarget articles
    previousArticle = if null prevArticles then Nothing else Just $ List.last prevArticles
    nextArticle = if null nextArticles then Nothing else Just $ List.head nextArticles

    tgtLink word (target, _) = mylink_ (destinationUrl $ destination target) word
    prevLink = fmap (tgtLink "[prev]") previousArticle
    nextLink = fmap (tgtLink "[next]") nextArticle


sortByDate :: [(a, Article [Text])] -> [(a, Article [Text])]
sortByDate = List.sortBy f
  where
    f (_,a1) (_,a2) = extractDate a2 `compare` extractDate a1

extractDate :: Article [Text] -> Maybe UTCTime
extractDate art = either (const Nothing) (date . extract)
  $ runAssembler
  $ json @PreambleData art Preamble

destImage :: OutputPrefix -> SourceLocation -> DestinationLocation
destImage prefix (FileSource path) =
  StaticFileDestination
    (Text.pack $ "/images/" <> takeFileName path)
    (prefix </> "images" </> takeFileName path)

data GenFileExtension
  = GenPngFile

extensionString :: GenFileExtension -> String
extensionString GenPngFile = ".png"

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
  nav_ [ class_ "articles-listing" ] $ do
    h2_ [ class_ "listing-callout"] $ Lucid.toHtml htext
    div_ [ class_ "articles-listing-list" ] $ do
      mconcat
        [ uncurry mainArticleLinkWithAnnotation p
        | p <- targets
        ]

siteGraphEchartZone :: Lucid.Html ()
siteGraphEchartZone =
  nav_ [ class_ "articles-graph" ] $ do
    h2_ [ class_ "listing-callout"] "site map"
    div_ [ id_ "echartzone" , style_ "width:800px;height:600px;"] mempty

mainArticleLinkWithAnnotation :: Target a -> Article [Text] -> Lucid.Html ()
mainArticleLinkWithAnnotation t art =
    div_ [ class_ $ mconcat [ "articles-listing-item", " ", publishStatusClass ] ] $ do
      span_ [ class_ "article-date" ] (toHtml formattedDate)
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

    statusIcon :: Lucid.Html ()
    statusIcon = case publicationStatus =<< buildinfo art of
      Nothing -> mempty
      Just Public -> mempty
      Just Upcoming -> span_ [ class_ "notice-upcoming" ] "(upcoming)"

    preamble :: Maybe PreambleData
    preamble =
      either (const Nothing) Just
      $ fmap extract
      $ runAssembler
      $ json @PreambleData art Preamble

preambleDateText :: PreambleData -> Maybe Text
preambleDateText preamble =
    (datetxt preamble) <|>
    (fmtUTC <$> date preamble)
  where
    fmtUTC :: UTCTime -> Text
    fmtUTC = Text.pack . formatTime defaultTimeLocale "%a, %d %b %Y"

articleSummary :: Article [Text] -> Lucid.Html ()
articleSummary art = when (isJust x) $ do
    div_ [ class_ "article-summary" ] $ do
      when (shouldShowStatsForArticle art) $ do
        articleStats art
      articleImage art
      maybe mempty (toHtmlRaw @Text . extract') x
  where
    x :: Maybe (Section PreRenderedHtml)
    x = fromRight Nothing $ runAssembler (lookupSection art Summary >>= traverse renderSection)

articleStats :: Article [Text] -> Lucid.Html ()
articleStats art = do
    div_ [ class_ "article-stats" ] $ do
      p_ $ toHtml $ List.intercalate " "
                  [ show wc, "words", "/"
                  , show . length . linkInfos $ infos, "links", "/"
                  , show . length . imageInfos $ infos, "images", "/"
                  , show . length . snippetInfos $ infos, "snippets"
                  ]
  where
    infos = analyzeArticle art

    wc :: Int
    wc = contentWordCount xs

    xs :: [Section [Text]]
    xs = fromRight [] $ runAssembler (getSections art MainContent)

articleImage :: Article [Text] -> Lucid.Html ()
articleImage art = do
    div_ [ class_ "article-image" ] $ do
      maybe mempty (\link -> img_ [ src_ link ] ) imgUrl
  where
    imgUrl :: Maybe Text
    imgUrl = case runAssembler getLink of
                Left _ -> Nothing
                Right x -> x

    getLink :: Assembler (Maybe Text)
    getLink = f <$> jsonm @TopicData art Topic

    f :: Maybe (Section TopicData) -> Maybe Text
    f sec = imageLink . extract =<< sec

assembleAtomEntry :: MetaExtraData -> DestinationLocation -> Article [Text] -> Assembler (Atom.Entry)
assembleAtomEntry extra dloc art = do
    summary <- fmap (compactSummary . extract) <$> lookupSection art Summary
    r <$> (extract <$> json @PreambleData art Preamble)
      <*> pure summary
  where
    r :: PreambleData -> Maybe Text -> Atom.Entry
    r preamble summary =
      let url = publishBaseURL extra <> destinationUrl dloc
          selfLink = (Atom.nullLink url) { Atom.linkRel = Just $ Left "alternate" }
          base =
            Atom.nullEntry
                (url)
                (Atom.TextString $ title preamble)
                (fmtUTC $ fromMaybe epochUTCTime $ date preamble)
       in base { Atom.entrySummary = fmap Atom.TextString summary
               , Atom.entryAuthors = [Atom.nullPerson { Atom.personName = author preamble}]
               , Atom.entryLinks = [selfLink]
               }

    fmtUTC = Text.pack . iso8601Show

assemblePreambleData :: Article [Text] -> Assembler (Maybe PreambleData)
assemblePreambleData art = do
  fmap extract <$> jsonm @PreambleData art Preamble

articlePreambleData :: Article [Text] -> Maybe PreambleData
articlePreambleData =
  join . hush . runAssembler . assemblePreambleData

assembleTopicData :: Article [Text] -> Assembler (Maybe TopicData)
assembleTopicData art = do
  fmap extract <$> jsonm @TopicData art Topic

articleTopicData :: Article [Text] -> Maybe TopicData
articleTopicData =
  join . hush . runAssembler . assembleTopicData

compactSummary :: [Text] -> Text
compactSummary = Text.strip . Text.intercalate " "

assembleCompactSummary :: Article [Text] -> Assembler Text
assembleCompactSummary art = do
  summary <- fmap extract <$> lookupSection art Summary
  pure $ maybe "" compactSummary summary

assembleTitle :: Article [Text] -> Assembler (Maybe Text)
assembleTitle art = do
  preamble <- fmap extract <$> jsonm @PreambleData art Preamble
  pure (title <$> preamble)

articleCompactSummary :: Article [Text] -> Maybe Text
articleCompactSummary =
  hush . runAssembler . assembleCompactSummary

articleTitle :: Article [Text] -> Maybe Text
articleTitle =
  join . hush . runAssembler . assembleTitle

epochUTCTime :: UTCTime
epochUTCTime = UTCTime (fromOrdinalDate 1970 1) (secondsToDiffTime 0)