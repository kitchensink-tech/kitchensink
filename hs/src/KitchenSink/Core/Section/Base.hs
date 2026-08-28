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
    | Callout
    | Faq
    | Pricing
    | Dataset Name
    | Library Name
    | Extension ext
    | --
      GeneratorInstructions
    deriving (Show, Eq, Ord)

data Format
    = Cmark
    | Dhall
    | Mustache
    | -- | tramaj, expression root: evaluates to @{format, contents}@
      TramajJson
    | -- | tramaj, element root: evaluates to a document tree
      TramajDoc
    | -- | tramaj, library, for imports only
      TramajLib
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
