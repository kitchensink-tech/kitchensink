{-# LANGUAGE DeriveFunctor #-}
module KitchenSink.Blog.Site where

import Data.Text (Text)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Target
import KitchenSink.Blog.Section

data Article a
  = Article FilePath [Section a]
  deriving (Show, Eq, Ord, Functor)

-- A lens-like over-function that effectfully alters sections of an article.
overSections :: (Applicative t) => (Section a -> t (Section b)) -> Article a -> t (Article b)
overSections f (Article p xs) =
  Article p <$> traverse f xs

data VideoFile = VideoFile
  deriving (Show, Eq, Ord)

data RawFile = RawFile
  deriving (Show, Eq, Ord)

data CssFile = CssFile
  deriving (Show, Eq, Ord)

data JsFile = JsFile
  deriving (Show, Eq, Ord)

data Image = Image
  deriving (Show, Eq, Ord)

data DotSourceFile = DotSourceFile
  deriving (Show, Eq, Ord)

data Site = Site
  { articles :: [ Sourced (Article [Text]) ]
  , images :: [ Sourced Image ]
  , videoFiles :: [ Sourced VideoFile ]
  , cssFiles :: [ Sourced CssFile ]
  , jsFiles :: [ Sourced JsFile ]
  , dotSourceFiles :: [ Sourced DotSourceFile ]
  , rawFiles :: [ Sourced RawFile ]
  }
  deriving (Show, Eq, Ord)

countSources :: Site -> Int
countSources s = sum
  [ length $ articles s
  , length $ images s
  , length $ videoFiles s
  , length $ cssFiles s
  , length $ jsFiles s
  , length $ dotSourceFiles s
  , length $ rawFiles s
  ]
