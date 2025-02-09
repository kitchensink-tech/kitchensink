{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

module KitchenSink.Engine.SiteLoader (module KitchenSink.Core.Build.Site, loadSite, LogMsg (..)) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LByteString
import Data.Either (fromRight)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (Void)
import Dhall
import Dhall.Context qualified as Context
import Dhall.Core qualified as Core
import Dhall.JSON (CompileError, dhallToJSON)
import Dhall.JSONToDhall (defaultConversion, dhallFromJSON, inferSchema, schemaToDhallType)
import Dhall.Map qualified as Dhall
import Dhall.Map qualified as DhallMap
import Dhall.Src (Src)
import Lens.Family
import System.Directory (listDirectory)
import System.FilePath.Posix (takeExtension, takeFileName, (</>))
import Text.Megaparsec (runParser)
import Text.Mustache qualified as Mustache
import Text.Parsec qualified as Parsec
import Prelude (Integer, succ, (||))

import Control.Monad.State

import KitchenSink.Core.Build.Site
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

data LogMsg ext
    = LoadArticle FilePath
    | LoadImage FilePath
    | LoadVideo FilePath
    | LoadRaw FilePath
    | LoadDocument FilePath
    | LoadCss FilePath
    | LoadWebfont FilePath
    | LoadJs FilePath
    | LoadHtml FilePath
    | LoadDotSource FilePath
    | EvalSection FilePath (SectionType ext) Format
    deriving (Show)

type Loader ext a = (LogMsg ext -> IO ()) -> FilePath -> IO (Sourced a)

-- TODO: consider adding some LoadedArticle type to:
-- - distinguish article that have just been parsed from pre-procssed-via Dhall
-- - return some extra structure about the Article like:
--   * dependencies using a specific section
--   * dependencies between sections? or between articles?
--   * dependencies to external query widgets or params?
--   * references to generated datasets (e.g., `curl a page, use as input to other place`)
loadArticle :: FilePath -> [(Text, Text)] -> [ExtraSectionType ext] -> Loader ext (Article ext [Text])
loadArticle dhallRoot vars extras trace path = do
    trace $ LoadArticle path
    eart <- runParser (article extras path) path <$> Text.readFile path
    case eart of
        Left err -> throwIO err
        Right art -> Sourced (FileSource path) <$> evalSections art
  where
    env = EvalEnv path dhallRoot vars trace
    evalSections art = evalStateT (overSections (evalSection env) art) newState

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

data EvalEnv ext
    = EvalEnv
    { path :: FilePath
    , dhallRoot :: FilePath
    , vars :: [(Text, Text)]
    , trace :: LogMsg ext -> IO ()
    }

data EvalError
    = UnsupportedReturnFormat Text
    | DhallRuntimeError CompileError
    | DhallResultJsonDecodeError String
    | MalformedJSONDataset Name String
    | MalformedJSONGeneratorInstructions String
    | MustacheCompileError Parsec.ParseError
    deriving (Show, Exception)

type DatasetCells =
    Map Name Aeson.Value

data EvalState = EvalState
    { sectionNumber :: Integer
    , datasets :: DatasetCells
    }

newState :: EvalState
newState = EvalState 0 Map.empty

type Eval a = StateT EvalState IO a

evalSection :: EvalEnv ext -> Section ext [Text] -> Eval (Section ext [Text])
evalSection env s = do
    x <- sectionStep env s
    incrementSectionNumber
    pure x

incrementSectionNumber :: Eval ()
incrementSectionNumber = modify f
  where
    f st0 = st0{sectionNumber = succ (sectionNumber st0)}

insertDatasetContents :: Name -> Aeson.Value -> Eval ()
insertDatasetContents k val = modify f
  where
    f st0 = st0{datasets = Map.insert k val (datasets st0)}

sectionStep :: forall ext. EvalEnv ext -> Section ext [Text] -> Eval (Section ext [Text])
sectionStep env x@(Section t fmt body) = do
    st0 <- get
    liftIO $ env.trace $ EvalSection env.path t fmt
    exec st0
  where
    exec :: EvalState -> Eval (Section ext [Text])
    exec st0 = case (t, fmt) of
        (_, Mustache) -> do
            let jsonDataset = Aeson.toJSON st0.datasets
            let template = Mustache.compileTemplate "(section)" (Text.unlines body)
            case template of
                Left err -> liftIO $ throwIO $ MustacheCompileError err
                Right tpl -> do
                    let contents = Mustache.substitute tpl jsonDataset
                    pure $ Section t Cmark [contents]
        (_, Dhall) -> do
            let jsonDataset = Aeson.toJSON st0.datasets
            -- prepare kitchensink expression
            let dhallDataset = dhallFromJSON defaultConversion (schemaToDhallType $ inferSchema jsonDataset) jsonDataset
            let sectionNumExpr = Core.Annot (Core.IntegerLit st0.sectionNumber) (Core.Integer)
            let textExpr v = Core.TextLit (Core.Chunks [] v)
            let pathExpr = Core.Annot (Core.TextLit (Core.Chunks [] $ Text.pack env.path)) (Core.Text)
            let errorExpr = Core.Annot (Core.TextLit (Core.Chunks [] "could not load datasets into Dhall")) (Core.Text)
            let varListExprs = [(k, Core.makeRecordField (textExpr v)) | (k, v) <- env.vars]
            let varsExprc =
                    Core.RecordLit
                        (DhallMap.fromList varListExprs)
            let ksExpr =
                    Core.RecordLit
                        $ DhallMap.fromList
                            [ ("file", Core.makeRecordField pathExpr)
                            , ("sectionNum", Core.makeRecordField sectionNumExpr)
                            , ("datasets", Core.makeRecordField $ fromRight errorExpr dhallDataset)
                            , ("vars", Core.makeRecordField varsExprc)
                            ]
            let ctx0 =
                    Context.empty
                        & Context.insert "kitchensink" ksExpr
            let sub0 = Dhall.fromList [("kitchensink", ksExpr)]
            -- eval dhall expression
            let setts =
                    defaultInputSettings
                        & Dhall.sourceName .~ (env.path <> " (section)")
                        & Dhall.rootDirectory .~ env.dhallRoot
                        & Dhall.evaluateSettings . substitutions .~ sub0
                        & Dhall.evaluateSettings . startingContext .~ ctx0
            de <- liftIO $ inputExprWithSettings setts (Text.unlines body) :: Eval (Core.Expr Src Void)

            -- turn expression into a parsed result, using JSON as an intermediary parser
            dj <- case dhallToJSON de of
                Left err -> liftIO $ throwIO $ DhallRuntimeError err
                Right jvalue -> pure $ jvalue
            case Aeson.fromJSON dj of
                Aeson.Error err ->
                    liftIO $ throwIO $ DhallResultJsonDecodeError err
                Aeson.Success (DhallJsonContents obj) ->
                    liftIO $ pure $ Section t Json [Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode obj]
                Aeson.Success (DhallTextContents newFormat contents) ->
                    case newFormat of
                        "cmark" -> pure $ Section t Cmark contents
                        "html" -> pure $ Section t TextHtml contents
                        unsupportedFmt -> liftIO $ throwIO $ UnsupportedReturnFormat $ "unknwon returned Dhall format: " <> unsupportedFmt
        (Dataset name, Json) -> do
            case (Aeson.eitherDecode $ LByteString.fromStrict $ Text.encodeUtf8 $ Text.unlines body) of
                Right v -> insertDatasetContents name v
                Left err -> liftIO $ throwIO $ MalformedJSONDataset name err
            pure x
        (Dataset name, _) -> do
            insertDatasetContents name (Aeson.String $ Text.unlines body)
            pure x
        (GeneratorInstructions, Json) -> do
            let jsonDataset = Aeson.toJSON st0.datasets
            case (Aeson.eitherDecode @GeneratorInstructionsData $ LByteString.fromStrict $ Text.encodeUtf8 $ Text.unlines body) of
                Left err -> liftIO $ throwIO $ MalformedJSONGeneratorInstructions err
                Right gen ->
                    if isJust gen.stdin_json || isJust gen.stdin
                        then pure x
                        else
                            pure
                                $ Section
                                    GeneratorInstructions
                                    Json
                                    [Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode $ gen{stdin_json = Just jsonDataset}]
        _ ->
            pure x

loadImage :: Loader a Image
loadImage trace path = do
    trace $ LoadImage path
    pure $ (Sourced (FileSource path) Image)

loadAudio :: Loader a AudioFile
loadAudio trace path = do
    trace $ LoadVideo path
    pure $ (Sourced (FileSource path) AudioFile)

loadVideo :: Loader a VideoFile
loadVideo trace path = do
    trace $ LoadVideo path
    pure $ (Sourced (FileSource path) VideoFile)

loadRaw :: Loader a RawFile
loadRaw trace path = do
    trace $ LoadRaw path
    pure $ (Sourced (FileSource path) RawFile)

loadDocument :: Loader a DocumentFile
loadDocument trace path = do
    trace $ LoadDocument path
    pure $ (Sourced (FileSource path) DocumentFile)

loadCss :: Loader a CssFile
loadCss trace path = do
    trace $ LoadCss path
    pure $ (Sourced (FileSource path) CssFile)

loadFont :: Loader a WebfontFile
loadFont trace path = do
    trace $ LoadWebfont path
    pure $ (Sourced (FileSource path) WebfontFile)

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

loadSite ::
    FilePath ->
    [(Text, Text)] ->
    [ExtraSectionType ext] ->
    (LogMsg ext -> IO ()) ->
    FilePath ->
    IO (Site ext)
loadSite dhallRoot vars extras trace dir = do
    paths <- listDirectory dir
    Site
        <$> articlesM paths
        <*> imagesM paths
        <*> videosM paths
        <*> audiosM paths
        <*> cssM paths
        <*> fontsM paths
        <*> jsM paths
        <*> htmlM paths
        <*> dotsM paths
        <*> rawsM paths
        <*> docsM paths
  where
    articlesM paths =
        traverse (loadArticle dhallRoot vars extras trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".md", ".cmark"]]
    imagesM paths =
        traverse (loadImage trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".jpg", ".jpeg", ".png"]]
    cssM paths =
        traverse (loadCss trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".css"]]
    fontsM paths =
        traverse (loadFont trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".ttf", ".woff2"]]
    jsM paths =
        traverse (loadJs trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".js"]]
    htmlM paths =
        traverse (loadHtml trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".html"]]
    dotsM paths =
        traverse (loadDotSource trace)
            $ [dir </> p | p <- paths, takeExtension p == ".dot"]
    videosM paths =
        traverse (loadVideo trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".webm", ".mp4"]]
    audiosM paths =
        traverse (loadAudio trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".ogg", ".mp3", ".wav", ".midi", ".flac"]]
    rawsM paths =
        traverse (loadRaw trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".txt", ".csv", ".json", ".dhall"], takeFileName p /= "kitchen-sink.json"]
    docsM paths =
        traverse (loadDocument trace)
            $ [dir </> p | p <- paths, takeExtension p `List.elem` [".pdf"]]
