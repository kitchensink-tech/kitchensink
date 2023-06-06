{-# LANGUAGE DeriveAnyClass #-}
module KitchenSink.Engine.SiteLoader (module KitchenSink.Core.Build.Site, loadSite, LogMsg(..))where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), withObject, (.:))
import Prelude (Integer,succ)
import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import Text.Megaparsec (runParser)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>), takeExtension, takeFileName)
import Dhall
import Dhall.Src (Src)
import Data.Void (Void)
import Dhall.JSON (dhallToJSON, CompileError)
import qualified Dhall.Map as Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Context as Context
import Lens.Family

import Control.Monad.State

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

-- TODO: consider adding some LoadedArticle type to:
-- - distinguish article that have just been parsed from pre-procssed-via Dhall
-- - return some extra structure about the Article like:
--   * dependencies using a specific section
--   * dependencies between sections? or between articles?
--   * dependencies to external query widgets or params?
--   * references to generated datasets (e.g., `curl a page, use as input to other place`)
loadArticle :: [ExtraSectionType ext] -> Loader ext (Article ext [Text])
loadArticle extras trace path = do
  trace $ LoadArticle path
  eart <- runParser (article extras path) path <$> Text.readFile path
  case eart of
    Left err -> throwIO err
    Right art -> Sourced (FileSource path) <$> evalSections art

  where
    evalSections art = evalStateT (overSections (evalSection path trace) art) newState

data DhallResult
  = DhallTextContents Text [Text]
  | DhallJsonContents Aeson.Value
  deriving (Show)

instance FromJSON DhallResult where
  parseJSON = withObject "DhallResult" $ \obj -> do
                format <- obj .: "format"
                case format of
                  "json" -> DhallJsonContents <$> obj .: "contents"
                  _ -> DhallTextContents format <$> obj .: "contents"

data EvalError
  = UnsupportedReturnFormat Text
  | DhallRuntimeError CompileError
  | DhallResultJsonDecodeError String
  deriving (Show, Exception)

data EvalState = EvalState {
    _sectionNuber :: Integer
  }

newState :: EvalState
newState = EvalState 0

type Eval a = StateT EvalState IO a

evalSection :: FilePath -> (LogMsg ext -> IO ()) -> Section ext [Text] -> Eval (Section ext [Text])
evalSection path trace x@(Section t fmt body) = do
  st0 <- get
  ret <- liftIO $ do
    trace $ EvalSection path t fmt
    exec st0
  incrementPage
  pure ret
  where
    incrementPage = modify (\st0 -> EvalState $ succ (_sectionNuber st0))

    exec (EvalState sectionNum) = case fmt of
        Dhall -> do
          let sectionNumExpr = Core.Annot (Core.IntegerLit sectionNum) (Core.Integer)
          let pathExpr = Core.Annot (Core.TextLit (Core.Chunks [] $ Text.pack path)) (Core.Text)
          let ksExpr = Core.RecordLit $ Dhall.fromList [("file", Core.makeRecordField pathExpr),("sectionNum", Core.makeRecordField sectionNumExpr)]
          let ctx0 = Context.empty
                      & Context.insert "kitchensink" ksExpr
          let sub0 = Dhall.fromList [ ("kitchensink", ksExpr) ]
          let setts = defaultInputSettings
                      & Dhall.sourceName .~ (path <> " (section)")
                      & Dhall.evaluateSettings . substitutions .~ sub0
                      & Dhall.evaluateSettings . startingContext .~ ctx0
          de <- inputExprWithSettings setts (Text.unlines body) :: IO (Core.Expr Src Void)
          dj <- case dhallToJSON de of
            Left err -> throwIO $ DhallRuntimeError err
            Right jvalue -> pure $ jvalue
    
          case Aeson.fromJSON dj of
            Aeson.Error err -> throwIO $ DhallResultJsonDecodeError err
            Aeson.Success (DhallJsonContents obj) -> pure $ Section t Json [Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode obj]
            Aeson.Success (DhallTextContents format contents) ->
              case format of
                "cmark" -> pure $ Section t Cmark contents
                "html" -> pure $ Section t TextHtml contents
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
                     $ [ dir </> p | p <- paths, takeExtension p `List.elem` [".txt", ".csv", ".json", ".dhall"] , takeFileName p /= "kitchen-sink.json"]
