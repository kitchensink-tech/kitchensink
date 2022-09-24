{-# LANGUAGE BangPatterns #-}
module KitchenSink.Blog.Target (Target(..), ProductionRule(..), Url, DestinationLocation(..), destinationUrl, SourceLocation(..), Sourced(..), Assembler(..), AssemblerError(..), copyFrom, outputTarget, produceTarget, execIO, execCmd, Tracer, Trace(..))
  where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import System.Directory (copyFile)
import System.Exit (ExitCode(..))
import System.Process.ByteString(readProcessWithExitCode)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Assembler
import KitchenSink.Blog.Generator

data ProductionRule
  = ProduceAssembler (Assembler LText.Text)
  | ProduceGenerator (Generator ByteString.ByteString)
  | ProduceFileCopy (Sourced ())
  deriving Show

data Target = Target
  { destination :: DestinationLocation
  , productionRule :: ProductionRule
  } deriving Show

data SourceLocation = FileSource FilePath
  deriving (Show, Eq, Ord)

data Sourced a = Sourced { location :: SourceLocation, obj :: a }
  deriving (Show, Eq, Ord)

type Url = Text

data DestinationLocation
  = StaticFileDestination Url FilePath
  | VirtualFileDestination Url FilePath
  deriving (Show, Eq, Ord)

destinationUrl :: DestinationLocation -> Url
destinationUrl (StaticFileDestination u _) = u
destinationUrl (VirtualFileDestination u _)  = u

outputTarget :: Target -> IO ByteString
outputTarget t = do
  case productionRule t of
    ProduceAssembler assembler -> do
      let ret = LByteString.toStrict . LText.encodeUtf8 <$> runAssembler assembler
      either throwIO pure ret
    ProduceGenerator generator -> do
      ret <- runGenerator generator
      either throwIO pure ret
    ProduceFileCopy (Sourced (FileSource src) _) -> do
      ByteString.readFile src

produceTarget :: Tracer -> Target -> IO ()
produceTarget trace t = do
  case productionRule t of
    ProduceAssembler assembler -> do
      trace $ Assembling dest
      case runAssembler assembler of
        Right s -> LText.writeFile dest s
        Left e  -> throwIO e
    ProduceGenerator generator -> do
      trace $ Generating dest
      ret <- runGenerator generator
      case ret of
        Right s -> ByteString.writeFile dest s
        Left e  -> throwIO e
    ProduceFileCopy (Sourced (FileSource src) _) -> do
      trace $ Copying dest
      copyFile src dest
  where
    dest = case destination t of
             (StaticFileDestination _ d) -> d
             (VirtualFileDestination _ d) -> d

copyFrom :: SourceLocation -> ProductionRule
copyFrom path = ProduceFileCopy (Sourced path ())

execIO :: IO ByteString -> ProductionRule
execIO io = ProduceGenerator (Generator $ io >>= pure . pure)

execCmd :: Tracer -> FilePath -> [String] -> ByteString -> ProductionRule
execCmd trace path args input = ProduceGenerator $ Generator $ do
  trace $ Executing path args
  (code, !out, !err) <- readProcessWithExitCode path args input
  pure $ case code of
    ExitFailure n -> Left (GeneratorError (show n) err)
    _             -> Right out


data Trace
  = Executing FilePath [String]
  | Copying FilePath
  | Generating FilePath
  | Assembling FilePath
  deriving Show

type Tracer = Trace -> IO ()
