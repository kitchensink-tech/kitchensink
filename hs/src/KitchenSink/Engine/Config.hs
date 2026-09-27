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

{- | One statically-configured user of the @config@ auth provider.
@passwordHash@ must be a hash produced by
"KitchenSink.Engine.Auth" (@kitchen-sink hash-password@) -- never a plaintext
password.
-}
data AuthUser = AuthUser
    { username :: Text
    , passwordHash :: Text
    }
    deriving (Generic, Show)
instance FromJSON AuthUser
instance ToJSON AuthUser

{- | Where an @auth@ section's credentials come from. One constructor today
(@config@: a static user list right in @kitchen-sink.json@); a later source
(a sqlite users table, a trusted reverse-proxy header) is a new constructor
here, not a rewrite of the callers -- see "KitchenSink.Engine.Auth".
-}
newtype AuthProviderConfig
    = ConfigProvider {users :: [AuthUser]}
    deriving (Generic, Show)
instance FromJSON AuthProviderConfig
instance ToJSON AuthProviderConfig

{- | The @auth@ stanza of @kitchen-sink.json@: enables authentication for
dynamic pages whose @=base:build-info.json@ declares @"auth":"required"@
(see "KitchenSink.Engine.Dynamic" and "KitchenSink.Engine.Auth").
@cookieSecret@ signs the session cookie; keep it out of the served tree
(i.e. only in @kitchen-sink.json@, which is excluded from raw site files by
"KitchenSink.Engine.SiteLoader").
-}
data AuthConfig = AuthConfig
    { provider :: AuthProviderConfig
    , cookieSecret :: Text
    }
    deriving (Generic, Show)
instance FromJSON AuthConfig
instance ToJSON AuthConfig

data Config = Config
    { publishScript :: Maybe FilePath
    , commands :: [Command]
    , api :: ApiProxyConfig
    , datasources :: Maybe (Map Text DatasourceConfig)
    -- ^ request-time dynamic pages (@kitchen-sink serve --dynamic@); see
    -- "KitchenSink.Engine.Dynamic"
    , auth :: Maybe AuthConfig
    -- ^ authentication for dynamic pages; see "KitchenSink.Engine.Auth"
    }
    deriving (Generic, Show)
instance FromJSON Config
instance ToJSON Config
