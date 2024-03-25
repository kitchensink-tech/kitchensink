-- | Base types for the Kitchensink section-format.
module KitchenSink.Core.Section.Base (SectionType (..), Format (..), Section (..))
where

import KitchenSink.Prelude

data SectionType ext
    = BuildInfo
    | Preamble
    | Topic
    | Summary
    | MainContent
    | MainCss
    | TakenOff
    | Social
    | Glossary
    | Dataset Name
    | Extension ext
    | --
      GeneratorInstructions
    deriving (Show, Eq, Ord)

data Format
    = Cmark
    | Dhall
    | Json
    | TextHtml
    | Css
    | Csv
    | InMemory
    deriving (Show, Eq, Ord)

data Section ext payload
    = Section
    { sectionType :: (SectionType ext)
    , sectionFormat :: Format
    , sectionPayload :: payload
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
