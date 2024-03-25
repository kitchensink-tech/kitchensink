{- | This module exposes the key payloads that allow to convey data to the Engine.

JSON serialization instances exist for metadata consumption on frontend/api clients.
-}
module KitchenSink.Core.Section.Payloads
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List qualified as List
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import KitchenSink.Prelude

data PublicationStatus
    = Public
    | Upcoming
    | Archived
    deriving (Show, Eq, Generic)
instance FromJSON PublicationStatus
instance ToJSON PublicationStatus

data BuildInfoData = BuildInfoData
    { layout :: Text
    , publicationStatus :: Maybe PublicationStatus
    }
    deriving (Show, Eq, Generic)
instance FromJSON BuildInfoData
instance ToJSON BuildInfoData

type TopicName = Text

data TopicData = TopicData
    { topics :: [TopicName]
    , keywords :: [Text]
    , imageLink :: Maybe Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON TopicData
instance ToJSON TopicData

topicKeywords :: TopicData -> [Text]
topicKeywords d = List.nub $ topics d <> keywords d

data PreambleData = PreambleData
    { author :: Text
    , datetxt :: Maybe Text
    , date :: Maybe UTCTime
    , title :: Text
    , faviconUrl :: Maybe Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON PreambleData
instance ToJSON PreambleData

data SocialData = SocialData
    { twitter :: Maybe Text
    , linkedin :: Maybe Text
    , github :: Maybe Text
    , mastodon :: Maybe Text
    , cohost :: Maybe Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON SocialData
instance ToJSON SocialData

data GeneratorInstructionsData = GeneratorInstructionsData
    { cmd :: Text
    , args :: [Text]
    , stdin :: Maybe Text
    , target :: FilePath
    }
    deriving (Show, Eq, Generic)
instance FromJSON GeneratorInstructionsData
instance ToJSON GeneratorInstructionsData

data GlossaryTerm = GlossaryTerm
    { term :: Text
    , definition :: Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON GlossaryTerm
instance ToJSON GlossaryTerm

data GlossaryData = GlossaryData
    { glossary :: [GlossaryTerm]
    }
    deriving (Show, Eq, Generic)
instance FromJSON GlossaryData
instance ToJSON GlossaryData
