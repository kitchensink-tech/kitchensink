module KitchenSink.Core.Section.Base (SectionType(..), Format(..), Section(..))
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
  | GeneratorInstructions
  | Glossary
  | Extension ext
  deriving (Show, Eq, Ord)

data Format
  = Cmark
  | Dhall
  | Json
  | TextHtml
  | Css
  | InMemory
  deriving (Show, Eq, Ord)

data Section ext a
  =  Section (SectionType ext) Format a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
