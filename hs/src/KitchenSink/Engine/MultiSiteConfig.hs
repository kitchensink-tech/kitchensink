{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.MultiSiteConfig where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Dhall

import KitchenSink.Prelude
import KitchenSink.Engine.SiteConfig (SiteInfo)
import KitchenSink.Engine.Config (ApiProxyConfig)

type HostName = Text

data FileSourceStanza
  = FileSourceStanza
  { path :: FilePath
  }
  deriving (Generic, Show)
instance FromJSON FileSourceStanza
instance ToJSON FileSourceStanza
instance Dhall.FromDhall FileSourceStanza

data SourceStanza
  = NoFiles
  | FileSource FileSourceStanza
  deriving (Generic, Show)
instance FromJSON SourceStanza
instance ToJSON SourceStanza
instance Dhall.FromDhall SourceStanza

data TLSStanza = TLSStanza {
    pem :: FilePath
  , key :: FilePath
  , sniDomain :: Maybe HostName
  } deriving (Generic, Show)
instance FromJSON TLSStanza
instance ToJSON TLSStanza
instance Dhall.FromDhall TLSStanza

data SiteStanza = SiteStanza {
    -- todo: multiple domains
    -- todo: consider binding domains and TLS together
    domain      :: HostName
  , tls         :: Maybe TLSStanza
  , siteSource  :: SourceStanza
  , tmpDir      :: FilePath
  , site        :: SiteInfo
  , api         :: ApiProxyConfig
  } deriving (Generic, Show)
instance FromJSON SiteStanza
instance ToJSON SiteStanza
instance Dhall.FromDhall SiteStanza

data FallbackStanza
  = FallbackWithOminousError
  | FallbackSite SiteStanza
  deriving (Generic, Show)
instance FromJSON FallbackStanza
instance ToJSON FallbackStanza
instance Dhall.FromDhall FallbackStanza

data MultiSiteConfig = MultiSiteConfig {
    sites :: [SiteStanza]
  , fallback :: FallbackStanza
  } deriving (Generic, Show)
instance FromJSON MultiSiteConfig
instance ToJSON MultiSiteConfig
instance Dhall.FromDhall MultiSiteConfig
