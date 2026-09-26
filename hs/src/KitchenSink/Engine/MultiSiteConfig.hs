{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.MultiSiteConfig where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import KitchenSink.Engine.Config (ApiProxyConfig)
import KitchenSink.Engine.SiteConfig (SiteInfo)
import KitchenSink.Prelude

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

data SourceStanza
    = NoFiles
    | KitchenSinkDirectorySource KitchenSinkDirectorySourceStanza
    deriving (Generic, Show)
instance FromJSON SourceStanza
instance ToJSON SourceStanza

data CertificateFiles = CertificateFiles
    { pem :: FilePath
    , key :: FilePath
    }
    deriving (Generic, Show)
instance FromJSON CertificateFiles
instance ToJSON CertificateFiles

data CertificateSource
    = NoCertificates -- lazy way to force a {tag:/contents:} json serialization
    | CertificateFileSource CertificateFiles
    deriving (Generic, Show)
instance FromJSON CertificateSource
instance ToJSON CertificateSource

data TLSStanza = TLSStanza
    { sniDomains :: Maybe [HostName]
    , certificate :: CertificateSource
    }
    deriving (Generic, Show)
instance FromJSON TLSStanza
instance ToJSON TLSStanza

data SiteStanza = SiteStanza
    { domain :: HostName
    , extraDomains :: [HostName]
    , tls :: [TLSStanza]
    , site :: SourceStanza
    , api :: ApiProxyConfig
    }
    deriving (Generic, Show)
instance FromJSON SiteStanza
instance ToJSON SiteStanza

data FallbackStanza
    = FallbackWithOminousError
    | FallbackSite SiteStanza
    deriving (Generic, Show)
instance FromJSON FallbackStanza
instance ToJSON FallbackStanza

data MultiSiteConfig = MultiSiteConfig
    { services :: [SiteStanza]
    , fallback :: FallbackStanza
    }
    deriving (Generic, Show)
instance FromJSON MultiSiteConfig
instance ToJSON MultiSiteConfig
