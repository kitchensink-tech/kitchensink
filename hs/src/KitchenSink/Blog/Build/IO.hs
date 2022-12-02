module KitchenSink.Blog.Build.IO (produceTarget, outputTarget)
  where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy.IO as LText
import System.Directory (copyFile)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Assembler
import KitchenSink.Blog.Generator
import KitchenSink.Blog.Build.Target
import KitchenSink.Blog.Build.Trace

outputTarget :: Tracer -> Target a -> IO ByteString
outputTarget trace t = do
  case productionRule t of
    ProduceAssembler assembler -> do
      let ret = LByteString.toStrict . LText.encodeUtf8 <$> runAssembler assembler
      either throwIO pure ret
    ProduceGenerator fgenerator -> do
      ret <- runGenerator (fgenerator trace)
      either throwIO pure ret
    ProduceFileCopy (Sourced (FileSource src) _) -> do
      ByteString.readFile src

produceTarget :: Tracer -> Target a -> IO ()
produceTarget trace t = do
  case productionRule t of
    ProduceAssembler assembler -> do
      trace $ Assembling dest
      case runAssembler assembler of
        Right s -> LText.writeFile dest s
        Left e  -> throwIO e
    ProduceGenerator fgenerator -> do
      trace $ Generating dest
      ret <- runGenerator (fgenerator trace)
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
