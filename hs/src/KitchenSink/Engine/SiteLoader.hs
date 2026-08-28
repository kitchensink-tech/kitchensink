{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

module KitchenSink.Engine.SiteLoader (module KitchenSink.Core.Build.Site, loadSite, LogMsg (..)) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
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
import System.FilePath.Posix (dropExtension, takeExtension, takeFileName, (</>))
import Text.Megaparsec (runParser)
import Text.Mustache qualified as Mustache
import Text.Parsec qualified as Parsec
import Prelude (Integer, succ, (||))
import Control.Monad (foldM)
import qualified Tramaj.Ast
import qualified Tramaj.Eval

import Control.Monad.State

import KitchenSink.Core.Build.Site
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Engine.Templating (TemplatingError)
import KitchenSink.Engine.Templating qualified as Templating
import KitchenSink.Prelude

data LogMsg ext
    = LoadArticle FilePath
    | LoadTemplatingLibraryFile FilePath
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
loadArticle :: FilePath -> [(Text, Text)] -> [ExtraSectionType ext] -> Tramaj.Eval.LibraryTable -> Loader ext (Article ext [Text])
loadArticle dhallRoot vars extras globalLibs trace path = do
    trace $ LoadArticle path
    eart <- runParser (article extras path) path <$> Text.readFile path
    case eart of
        Left err -> throwIO err
        Right art -> Sourced (FileSource path) <$> evalSections art
  where
    env = EvalEnv path dhallRoot vars trace
    evalSections art = evalStateT (overSections (evalSection env) art) (newState globalLibs)

{- | Parses every @library.templating-lib@ section out of a set of
templating-library-only files (a @*.cmark-tramaj@ file, picked up by
'loadSite' and excluded from the site's articles) into one shared
'Tramaj.Eval.LibraryTable', which every article is then seeded with (see
'loadArticle').

Every name a file registers is nested under that file's own basename, so
@bob.cmark-tramaj@'s @icon@ library is only ever reachable as
@import(\"bob\/icon\", ...)@. Two files can therefore never collide on a name
-- the filesystem itself already guarantees two files in one directory don't
share a basename -- so 'DuplicateTemplatingLibrary' below can only ever fire
within one file, same as the ordinary in-article case.

Deliberately just parsing, not evaluating: a library's body is not run here
any more than an in-article @library.templating-lib@ section's body is run
where it is declared (see 'sectionStep') -- it is run later, wherever
something actually imports it and reads @.rendered@ off the result, against
whatever library table /that/ call site has. Which library file 'loadSite'
processes first therefore cannot matter for correctness; a genuine import
cycle (two libraries, from the same file or different ones, that import each
other) is caught by @tramaj@ itself as an 'Tramaj.Eval.EvalError'
('Tramaj.Eval.ImportCycle') when something finally evaluates that chain, not
by anything here.
-}
loadTemplatingLibraries ::
    (LogMsg ext -> IO ()) ->
    [ExtraSectionType ext] ->
    [FilePath] ->
    IO Tramaj.Eval.LibraryTable
loadTemplatingLibraries trace extras = foldM loadFile Map.empty
  where
    loadFile acc path = do
        trace $ LoadTemplatingLibraryFile path
        eart <- runParser (article extras path) path <$> Text.readFile path
        case eart of
            Left err -> throwIO err
            Right (Article _ secs) -> do
                fileTable <- foldM (registerSection path) Map.empty secs
                let prefix = Text.pack (dropExtension (takeFileName path))
                pure $ Map.union (Map.mapKeys ((prefix <> "/") <>) fileTable) acc

    registerSection path acc (Section (Library name) TramajLib body) = do
        case Templating.parseLibrarySection (Text.unlines body) of
            Left err -> throwIO $ TemplatingSectionError path err
            Right prog
                | Map.member name acc -> throwIO $ DuplicateTemplatingLibrary name path
                | otherwise -> pure $ Map.insert name prog acc
    registerSection _ acc _ = pure acc

{- | What an evaluated section (Dhall, or templating-lang in expression mode)
must answer with: a @format@ naming the concrete format the section is rewritten
to, plus its @contents@.
-}
data SectionEvalResult
    = TextContents Text [Text]
    | JsonContents Aeson.Value
    deriving (Show)

instance FromJSON SectionEvalResult where
    parseJSON = withObject "SectionEvalResult" $ \obj -> do
        format <- obj .: "format"
        case format of
            "json" -> JsonContents <$> obj .: "contents"
            _ -> TextContents format <$> (obj .: "contents" >>= textContents)
      where
        -- a single string is accepted as well as an array of lines, since
        -- writing `[ "…" ]` for a one-line result is pure noise
        textContents :: Aeson.Value -> Aeson.Types.Parser [Text]
        textContents (Aeson.String txt) = pure [txt]
        textContents v = parseJSON v

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
    | TemplatingSectionError FilePath TemplatingError
    | TemplatingResultJsonDecodeError FilePath String
    | -- | the same library name was registered twice within one
      -- @*.cmark-tramaj@ file; carries the name and the file
      DuplicateTemplatingLibrary Name FilePath
    deriving (Show, Exception)

type DatasetCells =
    Map Name Aeson.Value

data EvalState = EvalState
    { sectionNumber :: Integer
    , datasets :: DatasetCells
    , templatingLibraryTable :: Tramaj.Eval.LibraryTable
    }

newState :: Tramaj.Eval.LibraryTable -> EvalState
newState globalLibs = EvalState 0 Map.empty globalLibs

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

recordTemplatingLibrary :: Text -> Tramaj.Ast.Program -> Eval ()
recordTemplatingLibrary key x = modify f
  where
    f st0 = st0{templatingLibraryTable = g (templatingLibraryTable st0) }
    g libtable = Map.insert key x libtable 

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
                Aeson.Success result ->
                    rewriteSection "Dhall" result
        (_, TramajJson) -> do
            let ctx = Templating.buildContext env.path st0.sectionNumber env.vars st0.datasets
            case Templating.evalJsonSection st0.templatingLibraryTable ctx (Text.unlines body) of
                Left err -> liftIO $ throwIO $ TemplatingSectionError env.path err
                Right (prog,jvalue) -> case Aeson.fromJSON jvalue of
                    Aeson.Error err ->
                        liftIO $ throwIO $ TemplatingResultJsonDecodeError env.path err
                    Aeson.Success result -> do
                        recordTemplatingLibrary (Text.pack $ show $ st0.sectionNumber) prog
                        rewriteSection "tramaj-json" result
        (_, TramajDoc) -> do
            let ctx = Templating.buildContext env.path st0.sectionNumber env.vars st0.datasets
            case Templating.evalDocSection st0.templatingLibraryTable ctx (Text.unlines body) of
                Left err -> liftIO $ throwIO $ TemplatingSectionError env.path err
                Right (prog, html) -> do
                  recordTemplatingLibrary (Text.pack $ show $ st0.sectionNumber) prog
                  pure $ Section t TextHtml [html]
        (Library name, TramajLib) -> do
            case Templating.parseLibrarySection (Text.unlines body) of
                Left err -> liftIO $ throwIO $ TemplatingSectionError env.path err
                Right prog -> do
                  recordTemplatingLibrary (Text.pack $ show $ st0.sectionNumber) prog
                  recordTemplatingLibrary name prog
                  pure $ Section t TextHtml []
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

    {- | Rewrites an evaluated section into the concrete format its result asked
    for. Shared by the Dhall and the templating backends; @backend@ only names
    which one for the error message.

    A generated dataset cell is registered here too: the format-dispatching
    branches above match before @(Dataset name, Json)@ does, so without this a
    @=base:dataset.dhall my-name@ (or @.templating@) cell would be rewritten to
    JSON and then stay invisible to every later section.
    -}
    rewriteSection :: Text -> SectionEvalResult -> Eval (Section ext [Text])
    rewriteSection _ (JsonContents obj) = do
        case t of
            Dataset name -> insertDatasetContents name obj
            _ -> pure ()
        pure $ Section t Json [Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode obj]
    rewriteSection backend (TextContents newFormat contents) =
        case newFormat of
            "cmark" -> pure $ Section t Cmark contents
            "html" -> pure $ Section t TextHtml contents
            unsupportedFmt ->
                liftIO
                    $ throwIO
                    $ UnsupportedReturnFormat
                    $ "unknown returned " <> backend <> " format: " <> unsupportedFmt

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
    globalLibs <- loadTemplatingLibraries trace extras (libraryPaths paths)
    Site
        <$> articlesM globalLibs paths
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
    -- Library-only files: a dedicated extension (not merely a naming
    -- convention on top of .cmark/.md) so an ordinary article can never be
    -- mistaken for one, or vice versa. They generate no target of their own
    -- -- 'articlesM' below never sees them, since their extension doesn't
    -- match its filter -- and are instead parsed for their
    -- @library.templating-lib@ sections into one shared table every article
    -- can import from. Sorted only so 'LoadTemplatingLibraryFile' tracing is
    -- in a stable, deterministic order; which file is processed first does
    -- not otherwise matter (see 'loadTemplatingLibraries').
    libraryPaths paths = List.sort [dir </> p | p <- paths, takeExtension p == ".cmark-tramaj"]
    articlesM globalLibs paths =
        traverse (loadArticle dhallRoot vars extras globalLibs trace)
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
