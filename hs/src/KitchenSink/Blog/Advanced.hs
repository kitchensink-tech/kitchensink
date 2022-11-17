{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module KitchenSink.Blog.Advanced
  ( filecounts
  , TopicGraph(..)
  , topicsgraph
  , Node(..)
  , articleCMarks
  , analyzeArticle
  , ArticleInfos(..)
  , LinkInfo(..)
  , ImageInfo(..)
  , ExternalSitesInfo(..)
  , Tag
  , TopicStats(..)
  , buildTopicStats
  , allTags
  , SkyLine
  , SkyLineItem
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import GHC.Generics (Generic)
import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude (succ, (-))

import KitchenSink.Blog.Basics
import KitchenSink.Blog.Section
import KitchenSink.Blog.Site
import KitchenSink.Blog.Target
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.AssembleSections
import KitchenSink.Commonmark.Free as CMark
import KitchenSink.Blog.Destinations

data FileCount = FileCount {
    srctype :: Text
  , count :: Int
  }
  deriving (Generic, Show)
instance ToJSON FileCount

filecounts :: Site -> [FileCount]
filecounts site =
  [ FileCount "articles" (length $ articles site)
  , FileCount "images"   (length $ images site)
  , FileCount "videos" (length $ videoFiles site)
  , FileCount "raws" (length $ rawFiles site)
  , FileCount "css" (length $ cssFiles site)
  , FileCount "js" (length $ jsFiles site)
  , FileCount "dot" (length $ dotSourceFiles site)
  ]

data TopicStats = TopicStats {
    byTopic      :: Map Tag [(Target (), Article [Text])]
  , knownTargets :: [(Target (), Article [Text])]
  }

allTags :: TopicStats -> [Tag]
allTags = Map.keys . byTopic

buildTopicStats :: [Sourced (Article [Text])] -> (Sourced (Article [Text]) -> Target ()) -> TopicStats
buildTopicStats arts mkTarget =
    TopicStats indexByTopic [(mkTarget sa, a) | sa@(Sourced _ a) <- arts]
  where
    indexByTopic = Map.fromListWith (<>)
      $ [ (tag, [(mkTarget s, art)])
      | s@(Sourced _ art) <- arts
      , tag <- getTags art
      ]

    getTags :: Article [Text] -> [Tag]
    getTags art = either (const []) tags . runAssembler $ (f art)

    f :: Article [Text] -> Assembler TopicData
    f art = extract <$> json @TopicData art Topic

type NodeKey = Text
type URL = Text
data Node
  = TopicNode URL Int
  | ArticleNode URL Int
  | ImageNode URL
  | ExternalKitchenSinkSiteNode URL
  deriving (Generic, Show)
instance ToJSON Node
instance FromJSON Node
data TopicGraph = TopicGraph {
    nodes :: [(NodeKey, Node)]
  , edges :: [(NodeKey,NodeKey)]
  }
  deriving (Generic, Show)
instance ToJSON TopicGraph
instance FromJSON TopicGraph

data ExternalSitesInfo = ExternalSitesInfo {
    externalKitchenSinks :: [URL]
  }

topicsgraph :: ExternalSitesInfo -> TopicStats -> TopicGraph
topicsgraph external stats =
    TopicGraph
      (topicNodes <> articleNodes <> imagesNodes <> externalKSSitesNodes)
      (topicArticleEdges <> articleArticleEdges <> articleImageEdges <> articleExternalSiteEdges)
  where
    topicNodes,articleNodes,externalKSSitesNodes :: [(NodeKey, Node)]
    topicNodes = [ (topicKey t, TopicNode (destinationUrl $ destTag "" t) (length xs)) | (t,xs) <- Map.toList (byTopic stats) ]
    articleNodes = [ (articleKey t, ArticleNode (targetUrl t) histsize) | (t,histsize) <- uniqueTargetArticles ]
    imagesNodes = [ (imageKey url, ImageNode url) | url <- uniqueImages ]
    externalKSSitesNodes = [ (externalSiteKey url, ExternalKitchenSinkSiteNode url) | url <- externalKitchenSinks external]

    topicArticleEdges :: [(NodeKey, NodeKey)]
    topicArticleEdges =
        mconcat
        $ fmap (\(tag, xs) -> [ (topicKey tag, articleKey tgt) | (tgt,_) <- xs])
        $ Map.toList (byTopic stats)

    allLinks :: [(Target (), LinkInfo)]
    allLinks = do
      -- list monad!
      (from, a) <- knownTargets stats
      link <- linkInfos $ analyzeArticle a
      pure (from,link)

    articleArticleEdges :: [(NodeKey, NodeKey)]
    articleArticleEdges = do
      -- list monad!
      (from, link) <- allLinks
      toKey <- maybe [] (:[]) (lookupArticleLink link)
      pure (articleKey from, toKey)

    articleExternalSiteEdges :: [(NodeKey, NodeKey)]
    articleExternalSiteEdges = do
      -- list monad!
      (from, link) <- allLinks
      toKey <- maybe [] (:[]) (lookupExternalSiteLink link)
      pure (articleKey from, toKey)

    articleImageEdges :: [(NodeKey, NodeKey)]
    articleImageEdges = do
      -- list monad!
      ((from, _), infos) <- List.zip (knownTargets stats) analyses
      img <- imageInfos $ infos
      pure (articleKey from, imageKey $ imageURL img)

    topicKey t = "topic:" <> t
    articleKey t = "article:" <> targetUrl t
    imageKey t = "image:" <> t
    externalSiteKey t = "site:" <> t

    targetUrl t = destinationUrl (destination t)

    uniqueTargetArticles :: [(Target (),Int)]
    uniqueTargetArticles = do
      ((from, _), infos) <- List.zip (knownTargets stats) analyses
      pure (from, length $ histogram infos)

    analyses :: [ArticleInfos]
    analyses = fmap (analyzeArticle . snd) $ knownTargets stats

    uniqueImages :: [Text]
    uniqueImages = List.nub $ do
       infos <- analyses
       img <- imageInfos $ infos
       pure $ imageURL img

    lookupArticleLink :: LinkInfo -> Maybe NodeKey
    lookupArticleLink (LinkInfo url _) =
      fmap articleKey
      $ List.find (\t -> targetUrl t == url)
      $ fmap fst
      $ knownTargets stats

    lookupExternalSiteLink :: LinkInfo -> Maybe NodeKey
    lookupExternalSiteLink (LinkInfo url _) =
      fmap externalSiteKey
      $ List.find (\t -> t `Text.isPrefixOf` url)
      $ externalKitchenSinks external


data ArticleInfos = ArticleInfos {
    ast :: [CMark.Block ()]
  , linkInfos :: [LinkInfo]
  , imageInfos :: [ImageInfo]
  , snippetInfos :: [SnippetInfo]
  , histogram :: [Int]
  , skyline :: SkyLine
  } deriving (Show, Generic)
instance ToJSON ArticleInfos

data LinkInfo = LinkInfo { linkURL :: Text, linkText :: Text }
  deriving (Show, Generic)
instance ToJSON LinkInfo
instance FromJSON LinkInfo

data SnippetInfo = SnippetInfo { snippetContents :: Text, snippetType :: Text }
  deriving (Show, Generic)
instance ToJSON SnippetInfo
instance FromJSON SnippetInfo

data ImageInfo = ImageInfo { imageURL :: Text, imageText :: Text }
  deriving (Show, Generic)
instance ToJSON ImageInfo
instance FromJSON ImageInfo

articleCMarks :: Article [Text] -> [CMark.Block ()]
articleCMarks art =
  case runAssembler (getSections art MainContent >>= traverse dumpCMark . List.filter isCmark) of
    Left _ -> []
    Right xs -> fmap extract xs

  where
    isCmark (Section _ Cmark _) = True
    isCmark _ = False

analyzeArticle :: Article [Text] -> ArticleInfos
analyzeArticle art =
  let xs = articleCMarks art
  in ArticleInfos
        (xs)
        (mconcat [findLinksInSection x | x <- xs])
        (mconcat [findImagesInSection x | x <- xs])
        (mconcat [findSnippetsInSection x | x <- xs])
        (mconcat [sectionHistogram x | x <- xs])
        (mconcat [sectionSkyLine x | x <- xs])

findLinksInSection :: (CMark.Block a) -> [LinkInfo]
findLinksInSection s = do
  -- list monad!
  il <- blockInlines (s)
  Link dst ttl _ <- inlineChunks il
  pure $ LinkInfo dst ttl

findSnippetsInSection :: (CMark.Block a) -> [SnippetInfo]
findSnippetsInSection s = do
  -- list monad!
  b <- blockUniverse (s)
  CodeBlock typ_ raw <- blockChunks b
  pure $ SnippetInfo typ_ raw

findImagesInSection :: (CMark.Block a) -> [ImageInfo]
findImagesInSection s = do
  -- list monad!
  il <- blockInlines (s)
  CMark.Image dst ttl _ <- inlineChunks il
  pure $ ImageInfo dst ttl

sectionHistogram :: (CMark.Block a) -> [Int]
sectionHistogram s =
  fmap go $ blockInlines (s) >>= inlineUniverse
  where
    go il = List.sum [ f c | c <- inlineChunks il ]
    f x = case x of
            Str t -> Text.length t
            Link _ t _ -> Text.length t
            Code t -> Text.length t
            Entity _ -> 1
            EscapedChar _ -> 1
            _ -> 0

newtype SkyLine = SkyLine { skylineItems :: [SkyLineItem] }
  deriving Show
  deriving Generic
  deriving Semigroup via [SkyLineItem]
  deriving Monoid via [SkyLineItem]
instance ToJSON SkyLine
instance FromJSON SkyLine

data SkyLineItem
  = HeaderMark Text HeadingLevels
  | ImageMark Text Text
  | TextualMark TextWeight HeadingLevels
  deriving Show
  deriving Generic
instance ToJSON SkyLineItem
instance FromJSON SkyLineItem

type TextWeight = Int --todo newtype over Sum Int
type HeadingLevels = [Int] --todo newtype over nonempty list

sectionSkyLine :: (CMark.Block a) -> SkyLine
sectionSkyLine s =
    let u = s : blockUniplate s
    in SkyLine $ List.reverse $ snd $ foldSkylineBlocks [ c | b <- u , c <- blockChunks b ]
  where
    foldSkylineBlocks :: [CMark.BlockChunk a] -> (HeadingLevels, [SkyLineItem])
    foldSkylineBlocks = List.foldl' foldOneBlock ([0], [])

    foldOneBlock :: (HeadingLevels, [SkyLineItem]) -> CMark.BlockChunk a -> (HeadingLevels, [SkyLineItem])
    foldOneBlock (hdrs, xs) b =
      let hdrs' = case b of
                     Heading n _ -> updateHeaders n hdrs
                     _ -> hdrs
          item = case b of
                     Heading _ il ->
                       HeaderMark (mconcat $ fmap flattenText $ inlineChunks $ il) hdrs'
                     x ->
                       let directInlines = blockChunkInlines x
                           childrenInlines = List.concatMap blockInlines (blockChunkBlocks x)
                           allInlines = childrenInlines <> directInlines
                       in TextualMark (List.sum $ fmap go allInlines) hdrs'
      in (hdrs', item : xs)

    updateHeaders :: Int -> [Int] -> [Int]
    updateHeaders level counters =
      case List.drop (List.length counters - level) counters of
        [] -> 1 : counters
        cnt:levelcounters -> succ cnt : levelcounters
      
    go il = List.sum [ f c | c <- inlineChunks il ]
    f x = case x of
            Str t -> wc t
            Link _ t _ -> wc t
            Code t -> wc t
            Entity _ -> 1
            EscapedChar _ -> 1
            _ -> 0
    flattenText x = case x of
            Str t -> t
            Link _ t _ -> t
            Code t -> t
            _ -> mempty

    wc = List.length . List.filter (\x -> not $ List.elem x [ "", ":"] ) . Text.words
