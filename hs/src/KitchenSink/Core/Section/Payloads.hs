
module KitchenSink.Core.Section.Payloads
  where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON,ToJSON)
import Data.Text (Text)
import qualified Data.List as List
import Data.Time.Clock (UTCTime)

import KitchenSink.Prelude

data PublicationStatus
  = Public
  | Upcoming
  deriving (Show, Eq, Generic)
instance FromJSON PublicationStatus
instance ToJSON PublicationStatus

data BuildInfoData = BuildInfoData {
    layout :: Text
  , publicationStatus :: Maybe PublicationStatus
  } deriving (Show, Eq, Generic)
instance FromJSON BuildInfoData
instance ToJSON BuildInfoData

data TopicData = TopicData {
    tags :: [Text]
  , keywords :: [Text]
  , imageLink :: Maybe Text
  } deriving (Show, Eq, Generic)
instance FromJSON TopicData
instance ToJSON TopicData

topicKeywords :: TopicData -> [Text]
topicKeywords d = List.nub $ tags d <> keywords d

data PreambleData = PreambleData {
    author  :: Text
  , datetxt :: Maybe Text
  , date    :: Maybe UTCTime
  , title   :: Text
  , faviconUrl   :: Maybe Text
  } deriving (Show, Eq, Generic)
instance FromJSON PreambleData
instance ToJSON PreambleData

data SocialData = SocialData {
    twitter :: Maybe Text
  , linkedin :: Maybe Text
  , github :: Maybe Text
  , mastodon :: Maybe Text
  , cohost :: Maybe Text
  } deriving (Show, Eq, Generic)
instance FromJSON SocialData
instance ToJSON SocialData

data GeneratorInstructionsData = GeneratorInstructionsData {
    cmd :: Text
  , args :: [Text]
  , stdin :: Maybe Text
  , target :: FilePath
  } deriving (Show, Eq, Generic)
instance FromJSON GeneratorInstructionsData
instance ToJSON GeneratorInstructionsData

data GlossaryTerm = GlossaryTerm {
    term :: Text
  , definition :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON GlossaryTerm
instance ToJSON GlossaryTerm

data GlossaryData = GlossaryData {
    glossary :: [GlossaryTerm]
  } deriving (Show, Eq, Generic)
instance FromJSON GlossaryData
instance ToJSON GlossaryData

