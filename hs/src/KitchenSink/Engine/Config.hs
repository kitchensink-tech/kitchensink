{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Config where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
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

type HostName = Text
type PortNum = Int
type Prefix = Text

data TransportSecurity
    = UseHTTPS
    | UsePlainText
    deriving (Eq, Generic, Show)
instance FromJSON TransportSecurity
instance ToJSON TransportSecurity

data RewriteRule
    = NoRewrite
    | DropPrefix
    | RewritePrefix Prefix
    | RewritePrefixHost Prefix HostName
    deriving (Eq, Generic, Show)
instance FromJSON RewriteRule
instance ToJSON RewriteRule

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

data ApiProxyConfig
    = NoProxying
    | SlashApiProxy HostName PortNum
    | SlashApiProxyList [SlashApiProxyDirective]
    deriving (Generic, Show)
instance FromJSON ApiProxyConfig
instance ToJSON ApiProxyConfig

{- | One entry of the @datasources@ object of @kitchen-sink.json@: for now,
the only backend is a read-only sqlite file, bound as @sqlite@; a page's
@.sql@ datasets all query the datasource named @\"main\"@ (see
"KitchenSink.Engine.Dynamic" -- per-dataset datasource selection is not
implemented yet).
-}
newtype DatasourceConfig = DatasourceConfig
    { sqlite :: FilePath
    }
    deriving (Generic, Show)
instance FromJSON DatasourceConfig
instance ToJSON DatasourceConfig

data Config = Config
    { publishScript :: Maybe FilePath
    , commands :: [Command]
    , api :: ApiProxyConfig
    , datasources :: Maybe (Map Text DatasourceConfig)
    -- ^ request-time dynamic pages (@kitchen-sink serve --dynamic@); see
    -- "KitchenSink.Engine.Dynamic"
    }
    deriving (Generic, Show)
instance FromJSON Config
instance ToJSON Config
