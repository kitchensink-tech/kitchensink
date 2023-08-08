module KitchenSink.Engine.Utils where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as LByteString
import KitchenSink.Prelude

loadJSONFile :: FromJSON a => FilePath -> IO (Maybe a)
loadJSONFile path =
  decode <$> LByteString.readFile path
