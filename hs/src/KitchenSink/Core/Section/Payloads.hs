{- | This module exposes the key payloads that allow to convey data to the Engine.

JSON serialization instances exist for metadata consumption on frontend/api clients.
-}
module KitchenSink.Core.Section.Payloads
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as Aeson
import Data.Char (toLower)
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
    , robots :: Maybe Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON BuildInfoData
instance ToJSON BuildInfoData

type TopicName = Text

data TopicData = TopicData
    { topics :: [TopicName]
    , keywords :: [Text]
    , imageLink :: Maybe Text
    , imageAlt :: Maybe Text
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
    , updated :: Maybe UTCTime
    , title :: Text
    , faviconUrl :: Maybe Text
    , lang :: Maybe Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON PreambleData
instance ToJSON PreambleData

data SocialData = SocialData
    { twitter :: Maybe Text
    , linkedin :: Maybe Text
    , github :: Maybe Text
    , mastodon :: Maybe Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON SocialData
instance ToJSON SocialData

data GeneratorInstructionsData = GeneratorInstructionsData
    { cmd :: Text
    , args :: [Text]
    , stdin :: Maybe Text
    , stdin_json :: Maybe Value
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

-- | An inline admonition/callout box (note, warning, tip, ...) interleaved
-- with main-content sections. `calloutKind` is a free-form string mapped to
-- a CSS class (`callout-<kind>`); unrecognized kinds fall back to a default
-- style. JSON keys drop the `callout` prefix (`kind`/`title`/`body`) since
-- the Haskell field names would otherwise clash with `PreambleData.title`.
data CalloutData = CalloutData
    { calloutKind :: Text
    , calloutTitle :: Maybe Text
    , calloutBody :: Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON CalloutData where
    parseJSON = Aeson.genericParseJSON calloutJsonOptions
instance ToJSON CalloutData where
    toJSON = Aeson.genericToJSON calloutJsonOptions

calloutJsonOptions :: Aeson.Options
calloutJsonOptions = Aeson.defaultOptions{Aeson.fieldLabelModifier = calloutFieldName}

calloutFieldName :: String -> String
calloutFieldName s = case List.stripPrefix "callout" s of
    Just (c : cs) -> toLower c : cs
    _ -> s

data FaqItem = FaqItem
    { question :: Text
    , answer :: Text
    }
    deriving (Show, Eq, Generic)
instance FromJSON FaqItem
instance ToJSON FaqItem

newtype FaqData = FaqData
    { items :: [FaqItem]
    }
    deriving (Show, Eq, Generic)
instance FromJSON FaqData
instance ToJSON FaqData

-- | One column of a pricing/comparison table. `values` must line up
-- positionally with `pricingFeatures` in the enclosing 'PricingTableData'.
data PricingPlan = PricingPlan
    { name :: Text
    , price :: Text
    , values :: [Text]
    }
    deriving (Show, Eq, Generic)
instance FromJSON PricingPlan
instance ToJSON PricingPlan

data PricingTableData = PricingTableData
    { pricingFeatures :: [Text]
    , pricingPlans :: [PricingPlan]
    }
    deriving (Show, Eq, Generic)
instance FromJSON PricingTableData
instance ToJSON PricingTableData
