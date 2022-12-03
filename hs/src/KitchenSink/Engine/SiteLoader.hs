{-# LANGUAGE DeriveAnyClass #-}
module KitchenSink.Engine.SiteLoader (module KitchenSink.Core.Build.Site, loadSite, LogMsg(..))where

import Control.Exception (throwIO)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import Text.Megaparsec (runParser)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>), takeExtension)
import Dhall
import qualified Dhall.Map as Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Context as Context
import Lens.Family

import KitchenSink.Prelude
import KitchenSink.Core.Build.Site
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section

data LogMsg ext
  = LoadArticle FilePath
  | LoadImage FilePath
  | LoadVideo FilePath
  | LoadRaw FilePath
  | LoadCss FilePath
  | LoadJs FilePath
  | LoadHtml FilePath
  | LoadDotSource FilePath
  | EvalSection FilePath (SectionType ext) Format
  deriving Show

type Loader ext a = (LogMsg ext -> IO ()) -> FilePath -> IO (Sourced a)

loadArticle :: [ExtraSectionType ext] -> Loader ext (Article ext [Text])
loadArticle extras trace path = do
  trace $ LoadArticle path
  eart <- runParser (article extras path) path <$> Text.readFile path
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

evalSection :: FilePath -> (LogMsg ext -> IO ()) -> Section ext [Text] -> IO (Section ext [Text])
evalSection path trace x@(Section t fmt body) = do
  trace $ EvalSection path t fmt
  case fmt of
    Dhall -> do
      let pathExpr = Core.Annot (Core.TextLit (Core.Chunks [] $ Text.pack path)) (Core.Text)
      let ksExpr = Core.RecordLit $ Dhall.fromList [("file", Core.makeRecordField pathExpr)]
      let ctx0 = Context.empty
      let sub0 = Dhall.fromList [ ("kitchensink", ksExpr) ]
      let setts = defaultInputSettings
                  & Dhall.sourceName .~ (path <> " (section)")
                  & Dhall.evaluateSettings . substitutions .~ sub0
                  & Dhall.evaluateSettings . startingContext .~ ctx0
      dr <- inputWithSettings setts auto (Text.unlines body) :: IO DhallResult
      case format dr of
        "cmark" -> pure $ Section t Cmark (toList $ contents dr)
        "html" -> pure $ Section t TextHtml (toList $ contents dr)
        newfmt -> throwIO $ UnsupportedReturnFormat $ "unknwon returned Dhall format: " <> newfmt
    _ -> pure x

loadImage :: Loader a Image
loadImage trace path = do
  trace $ LoadImage path
  pure $ (Sourced (FileSource path) Image)

loadVideo :: Loader a VideoFile
loadVideo trace path = do
  trace $ LoadVideo path
  pure $ (Sourced (FileSource path) VideoFile)

loadRaw :: Loader a RawFile
loadRaw trace path = do
  trace $ LoadRaw path
  pure $ (Sourced (FileSource path) RawFile)

loadCss :: Loader a CssFile
loadCss trace path = do
  trace $ LoadCss path
  pure $ (Sourced (FileSource path) CssFile)

loadJs :: Loader a JsFile
loadJs trace path = do
  trace $ LoadJs path
  pure $ (Sourced (FileSource path) JsFile)

loadHtml :: Loader a HtmlFile
loadHtml trace path = do
  trace $ LoadHtml path
  pure $ (Sourced (FileSource path) HtmlFile)

loadDotSource :: Loader a DotSourceFile
loadDotSource trace path = do
  trace $ LoadDotSource path
  pure $ (Sourced (FileSource path) DotSourceFile)

loadSite :: [ExtraSectionType ext] -> (LogMsg ext -> IO ()) -> FilePath -> IO (Site ext)
loadSite extras trace dir = do
  paths <- listDirectory dir
  Site
    <$> articlesM paths
    <*> imagesM paths
    <*> videosM paths
    <*> cssM paths
    <*> jsM paths
    <*> htmlM paths
    <*> dotsM paths
    <*> rawsM paths
  where
    articlesM paths = traverse (loadArticle extras trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".md", ".cmark" ] ]
    imagesM paths = traverse (loadImage trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".jpg", ".jpeg", ".png" ] ]
    cssM paths = traverse (loadCss trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".css"] ]
    jsM paths = traverse (loadJs trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".js"] ]
    htmlM paths = traverse (loadHtml trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".html"] ]
    dotsM paths = traverse (loadDotSource trace)
                     $ [ dir </> p | p <- paths, takeExtension p == ".dot" ]
    videosM paths = traverse (loadVideo trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".webm", ".mp4"] ]
    rawsM paths = traverse (loadRaw trace)
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".txt"] ]
