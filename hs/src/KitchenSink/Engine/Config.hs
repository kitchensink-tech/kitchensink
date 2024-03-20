{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Config where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Dhall qualified
import GHC.Generics (Generic)

import KitchenSink.Prelude

data Command = Command
    { exe :: FilePath
    , display :: Text
    , handle :: Text
    }
    deriving (Generic, Show)
instance FromJSON Command
instance ToJSON Command
instance Dhall.FromDhall Command

type HostName = Text
type PortNum = Int
type Prefix = Text

data TransportSecurity
    = UseHTTPS
    | UsePlainText
    deriving (Eq, Generic, Show)
instance FromJSON TransportSecurity
instance ToJSON TransportSecurity
instance Dhall.FromDhall TransportSecurity

data RewriteRule
    = NoRewrite
    | DropPrefix
    | RewritePrefix Prefix
    | RewritePrefixHost Prefix HostName
    deriving (Eq, Generic, Show)
instance FromJSON RewriteRule
instance ToJSON RewriteRule
instance Dhall.FromDhall RewriteRule

data SlashApiProxyDirective
    = SlashApiProxyDirective
    { security :: TransportSecurity
    , prefix :: Prefix
    , rewrite :: RewriteRule
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

data Config = Config
    { publishScript :: Maybe FilePath
    , commands :: [Command]
    , api :: ApiProxyConfig
    }
    deriving (Generic, Show)
instance FromJSON Config
instance ToJSON Config
instance Dhall.FromDhall Config
