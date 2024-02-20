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

data KitchenSinkDirectorySourceStanza
  = KitchenSinkDirectorySourceStanza
  { path :: FilePath
  , metadata :: SiteInfo
  , dhallRoot :: Maybe FilePath
  , execRoot :: Maybe FilePath
  }
  deriving (Generic, Show)
instance FromJSON KitchenSinkDirectorySourceStanza
instance ToJSON KitchenSinkDirectorySourceStanza
instance Dhall.FromDhall KitchenSinkDirectorySourceStanza

data SourceStanza
  = NoFiles
  | KitchenSinkDirectorySource KitchenSinkDirectorySourceStanza
  deriving (Generic, Show)
instance FromJSON SourceStanza
instance ToJSON SourceStanza
instance Dhall.FromDhall SourceStanza

data CertificateFiles = CertificateFiles {
    pem :: FilePath
  , key :: FilePath
  }
  deriving (Generic, Show)
instance FromJSON CertificateFiles
instance ToJSON CertificateFiles
instance Dhall.FromDhall CertificateFiles

data CertificateSource
  = NoCertificates --lazy way to force a {tag:/contents:} json serialization
  | CertificateFileSource CertificateFiles
  deriving (Generic, Show)
instance FromJSON CertificateSource
instance ToJSON CertificateSource
instance Dhall.FromDhall CertificateSource

data TLSStanza = TLSStanza {
    sniDomains :: Maybe [HostName]
  , certificate :: CertificateSource
  } deriving (Generic, Show)
instance FromJSON TLSStanza
instance ToJSON TLSStanza
instance Dhall.FromDhall TLSStanza

data SiteStanza = SiteStanza {
    domain       :: HostName
  , extraDomains :: [HostName]
  , tls          :: [TLSStanza]
  , site         :: SourceStanza
  , api          :: ApiProxyConfig
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
    services :: [SiteStanza]
  , fallback :: FallbackStanza
  } deriving (Generic, Show)
instance FromJSON MultiSiteConfig
instance ToJSON MultiSiteConfig
instance Dhall.FromDhall MultiSiteConfig
