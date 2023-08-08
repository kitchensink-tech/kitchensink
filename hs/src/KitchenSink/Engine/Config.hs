{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.Config where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Dhall

import KitchenSink.Prelude

data Command = Command {
    exe  :: FilePath
  , display :: Text
  , handle :: Text
  } deriving (Generic, Show)
instance FromJSON Command
instance ToJSON Command
instance Dhall.FromDhall Command

type HostName = Text
type PortNum = Int
type Prefix = Text

data SlashApiProxyDirective
  = SlashApiProxyDirective
  { prefix :: Prefix
  , hostname :: HostName
  , portnum :: PortNum
  }
  deriving (Generic, Show)
instance FromJSON SlashApiProxyDirective
instance ToJSON SlashApiProxyDirective
instance Dhall.FromDhall SlashApiProxyDirective

data ApiProxyConfig
  = NoProxying
  | SlashApiProxy HostName PortNum
  | SlashApiProxyList [SlashApiProxyDirective]
  deriving (Generic, Show)
instance FromJSON ApiProxyConfig
instance ToJSON ApiProxyConfig
instance Dhall.FromDhall ApiProxyConfig

data Config = Config {
    publishScript :: Maybe FilePath
  , commands :: [Command]
  , api :: ApiProxyConfig
  } deriving (Generic, Show)
instance FromJSON Config
instance ToJSON Config
instance Dhall.FromDhall Config
