module KitchenSink.Engine.SiteBuilder (produceTarget, outputTarget)
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LByteString
import Data.Text.Lazy.Encoding qualified as LText
import Data.Text.Lazy.IO qualified as LText
import Data.Typeable (Typeable)
import System.Directory (copyFile)

import KitchenSink.Core.Assembler
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Build.Trace
import KitchenSink.Core.Generator
import KitchenSink.Prelude

outputTarget :: (Typeable ext, Show ext) => Tracer -> Target ext a -> IO ByteString
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

produceTarget :: (Typeable ext, Show ext) => Tracer -> Target ext a -> IO ()
produceTarget trace t = do
    case productionRule t of
        ProduceAssembler assembler -> do
            trace $ Assembling dest
            case runAssembler assembler of
                Right s -> LText.writeFile dest s
                Left e -> throwIO e
        ProduceGenerator fgenerator -> do
            trace $ Generating dest
            ret <- runGenerator (fgenerator trace)
            case ret of
                Right s -> ByteString.writeFile dest s
                Left e -> throwIO e
        ProduceFileCopy (Sourced (FileSource src) _) -> do
            trace $ Copying dest
            copyFile src dest
  where
    dest = case destination t of
        (StaticFileDestination _ d) -> d
        (VirtualFileDestination _ d) -> d
