module KitchenSink.Engine.Utils where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as LByteString
import KitchenSink.Prelude
import qualified Dhall
import System.FilePath.Posix (takeExtension)

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
