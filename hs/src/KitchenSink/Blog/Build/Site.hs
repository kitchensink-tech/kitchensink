module KitchenSink.Blog.Build.Site where

import Data.Text (Text)

import KitchenSink.Prelude
import KitchenSink.Blog.Build.Target
import KitchenSink.Core.Section

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

data HtmlFile = HtmlFile
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
  , htmlFiles :: [ Sourced HtmlFile ]
  , dotSourceFiles :: [ Sourced DotSourceFile ]
  , rawFiles :: [ Sourced RawFile ]
  }
  deriving (Show, Eq, Ord)

