{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.Utils where

import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as LByteString
import KitchenSink.Prelude
import qualified Dhall
import System.FilePath.Posix (takeExtension)
import System.FilePath.Posix ((</>))
import Options.Generic

loadConfigFile :: (FromJSON a, Dhall.FromDhall a) => FilePath -> IO (Maybe a)
loadConfigFile path = 
  case takeExtension path of
    ".dhall" -> loadDhallFile path
    _ -> loadJSONFile path

loadJSONFile :: FromJSON a => FilePath -> IO (Maybe a)
loadJSONFile path =
  decode <$> LByteString.readFile path

loadDhallFile :: Dhall.FromDhall a => FilePath -> IO (Maybe a)
loadDhallFile path =
  Dhall.inputFile Dhall.auto path

mio :: Maybe (IO ()) -> IO ()
mio = fromMaybe (pure ())

ksPath
  :: FilePath <?> "source directory"
  -> Maybe FilePath <?> "kitchen-sink.json file"
  -> FilePath
ksPath base preferred =
  let fallback = coerce base </> "kitchen-sink.json"
  in fromMaybe fallback (coerce preferred)

