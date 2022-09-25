{-# LANGUAGE DeriveAnyClass #-}
module KitchenSink.Blog.SiteLoader (module KitchenSink.Blog.Site, loadSite, LogMsg(..))where

import Control.Exception (throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char (newline)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>), takeExtension)
import Dhall (input, Vector)
import Dhall.Marshal.Decode (FromDhall, auto)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Target
import KitchenSink.Blog.Section
import KitchenSink.Blog.Site

data LogMsg
  = LoadArticle FilePath
  | LoadImage FilePath
  | LoadVideo FilePath
  | LoadRaw FilePath
  | LoadCss FilePath
  | LoadJs FilePath
  | LoadDotSource FilePath
  | EvalSection FilePath SectionType Format
  deriving Show

article :: FilePath -> Parser (Article [Text])
article path = Article path <$> (section `sepBy` newline)

type Loader a = (LogMsg -> IO ()) -> FilePath -> IO (Sourced a)

loadArticle :: Loader (Article [Text])
loadArticle trace path = do
  trace $ LoadArticle path
  eart <- runParser (article path) path <$> Text.readFile path
  case eart of
    Left err -> throwIO err
    Right art -> Sourced (FileSource path) <$> overSections (evalSection path trace) art

data DhallResult
  = DhallResult
  { contents :: Vector Text
  , format :: Text
  }
  deriving (Generic,Show)

instance FromDhall DhallResult

data EvalError = UnsupportedReturnFormat Text
  deriving (Show, Exception)

evalSection :: FilePath -> (LogMsg -> IO ()) -> Section [Text] -> IO (Section [Text])
evalSection path trace x@(Section t fmt body) = do
  trace $ EvalSection path t fmt
  case fmt of
    Dhall -> do
      dr <- input auto (Text.unlines body) :: IO DhallResult
      case format dr of
        "cmark" -> pure $ Section t Cmark (toList $ contents dr)
        "html" -> pure $ Section t TextHtml (toList $ contents dr)
        newfmt -> throwIO $ UnsupportedReturnFormat $ "unknwon returned Dhall format: " <> newfmt
    _ -> pure x

loadImage :: Loader Image
loadImage trace path = do
  trace $ LoadImage path
  pure $ (Sourced (FileSource path) Image)

loadVideo :: Loader VideoFile
loadVideo trace path = do
  trace $ LoadVideo path
  pure $ (Sourced (FileSource path) VideoFile)

loadRaw :: Loader RawFile
loadRaw trace path = do
  trace $ LoadRaw path
  pure $ (Sourced (FileSource path) RawFile)

loadCss :: Loader CssFile
loadCss trace path = do
  trace $ LoadCss path
  pure $ (Sourced (FileSource path) CssFile)

loadJs :: Loader JsFile
loadJs trace path = do
  trace $ LoadJs path
  pure $ (Sourced (FileSource path) JsFile)

loadDotSource :: Loader DotSourceFile
loadDotSource trace path = do
  trace $ LoadDotSource path
  pure $ (Sourced (FileSource path) DotSourceFile)

loadSite :: (LogMsg -> IO ()) -> FilePath -> IO Site
loadSite trace dir = do
  paths <- listDirectory dir
  Site
    <$> articlesM paths
    <*> imagesM paths
    <*> videosM paths
    <*> cssM paths
    <*> jsM paths
    <*> dotsM paths
    <*> rawsM paths
  where
    articlesM paths = traverse (loadArticle trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".md", ".cmark" ] ]
    imagesM paths = traverse (loadImage trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".jpg", ".jpeg", ".png" ] ]
    cssM paths = traverse (loadCss trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".css"] ]
    jsM paths = traverse (loadJs trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".js"] ]
    dotsM paths = traverse (loadDotSource trace)
                     $ [ dir </> p | p <- paths, takeExtension p == ".dot" ]
    videosM paths = traverse (loadVideo trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".webm", ".mp4"] ]
    rawsM paths = traverse (loadRaw trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".txt"] ]
