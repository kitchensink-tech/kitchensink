{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.Config where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import GHC.Generics (Generic)

import KitchenSink.Prelude

data Command = Command {
    exe  :: FilePath
  , display :: Text
  , handle :: Text
  } deriving (Generic, Show)
instance FromJSON Command
instance ToJSON Command

type HostName = Text
type PortNum = Int
type Prefix = Text

data ApiProxyConfig
  = NoProxying
  | SlashApiProxy HostName PortNum
  | SlashApiProxyList [(Prefix, HostName, PortNum)]
  deriving (Generic, Show)
instance FromJSON ApiProxyConfig
instance ToJSON ApiProxyConfig

data Config = Config {
    publishScript :: Maybe FilePath
  , commands :: [Command]
  , api :: ApiProxyConfig
  } deriving (Generic, Show)
instance FromJSON Config
instance ToJSON Config

loadJSONFile :: FromJSON a => FilePath -> IO (Maybe a)
loadJSONFile path =
  decode <$> LByteString.readFile path
