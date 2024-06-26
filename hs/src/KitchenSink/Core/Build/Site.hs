module KitchenSink.Core.Build.Site where

import Text.Megaparsec
import Text.Megaparsec.Char (newline)

import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

data Article ext a
    = Article FilePath [Section ext a]
    deriving (Show, Eq, Ord, Functor)

article :: [ExtraSectionType ext] -> FilePath -> Parser (Article ext [Text])
article extras path = Article path <$> (section extras `sepBy` newline)

-- A lens-like over-function that effectfully alters sections of an article.
overSections :: (Applicative t) => (Section ext a -> t (Section ext b)) -> Article ext a -> t (Article ext b)
overSections f (Article p xs) =
    Article p <$> traverse f xs

data AudioFile = AudioFile
    deriving (Show, Eq, Ord)

data VideoFile = VideoFile
    deriving (Show, Eq, Ord)

data RawFile = RawFile
    deriving (Show, Eq, Ord)

data DocumentFile = DocumentFile
    deriving (Show, Eq, Ord)

data CssFile = CssFile
    deriving (Show, Eq, Ord)

data WebfontFile = WebfontFile
    deriving (Show, Eq, Ord)

data JsFile = JsFile
    deriving (Show, Eq, Ord)

data HtmlFile = HtmlFile
    deriving (Show, Eq, Ord)

data Image = Image
    deriving (Show, Eq, Ord)

data DotSourceFile = DotSourceFile
    deriving (Show, Eq, Ord)

data Site ext = Site
    { articles :: [Sourced (Article ext [Text])]
    , images :: [Sourced Image]
    , videoFiles :: [Sourced VideoFile]
    , audioFiles :: [Sourced AudioFile]
    , cssFiles :: [Sourced CssFile]
    , webfontFiles :: [Sourced WebfontFile]
    , jsFiles :: [Sourced JsFile]
    , htmlFiles :: [Sourced HtmlFile]
    , dotSourceFiles :: [Sourced DotSourceFile]
    , rawFiles :: [Sourced RawFile]
    , docFiles :: [Sourced DocumentFile]
    }
    deriving (Show, Eq, Ord)
