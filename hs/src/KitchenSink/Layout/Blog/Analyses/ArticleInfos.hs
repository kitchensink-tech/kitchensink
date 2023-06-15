{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module KitchenSink.Layout.Blog.Analyses.ArticleInfos
  ( analyzeArticle
  , ArticleInfos(..)
  , HashTagInfo(..)
  , ImageInfo(..)
  , LinkInfo(..)
  , SnippetInfo(..)
  ) where

import qualified Data.List as List
import GHC.Generics (Generic)
import Data.Aeson (ToJSON,FromJSON)
import Data.Text (Text)

import KitchenSink.Layout.Blog.Extensions (Article)
import KitchenSink.Core.Build.Target (runAssembler)
import KitchenSink.Core.Section
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections
import KitchenSink.Commonmark.Free as CMark

import KitchenSink.Layout.Blog.Analyses.SkyLine

data ArticleInfos = ArticleInfos {
    ast :: [CMark.Block ()]
  , linkInfos :: [LinkInfo]
  , imageInfos :: [ImageInfo]
  , snippetInfos :: [SnippetInfo]
  , hashtagInfos :: [HashTagInfo]
  , skyline :: SkyLine
  } deriving (Show, Generic)
instance ToJSON ArticleInfos

data LinkInfo = LinkInfo { linkURL :: Text, linkText :: Text }
  deriving (Show, Generic)
instance ToJSON LinkInfo
instance FromJSON LinkInfo

data SnippetInfo = SnippetInfo { snippetType :: Text, snippetContents :: Text }
  deriving (Show, Generic)
instance ToJSON SnippetInfo
instance FromJSON SnippetInfo

data ImageInfo = ImageInfo { imageURL :: Text, imageText :: Text }
  deriving (Show, Generic)
instance ToJSON ImageInfo
instance FromJSON ImageInfo

data HashTagInfo = HashTagInfo { hashtagValue :: Text }
  deriving (Show, Generic, Eq, Ord)
instance ToJSON HashTagInfo
instance FromJSON HashTagInfo

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
        (List.nub $ mconcat [findHashTagsInSection x | x <- xs])
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

findHashTagsInSection :: (CMark.Block a) -> [HashTagInfo]
findHashTagsInSection s = do
  -- list monad!
  il <- blockInlines (s)
  HashTag txt <- inlineChunks il
  pure $ HashTagInfo txt

findImagesInSection :: (CMark.Block a) -> [ImageInfo]
findImagesInSection s = do
  -- list monad!
  il <- blockInlines (s)
  CMark.Image dst ttl _ <- inlineChunks il
  pure $ ImageInfo dst ttl
