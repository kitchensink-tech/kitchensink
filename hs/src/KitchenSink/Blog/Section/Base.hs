module KitchenSink.Blog.Section.Base (SectionType(..), Format(..), Section(..))
where

import KitchenSink.Blog.Prelude

data SectionType
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
  deriving (Show, Eq, Ord)

data Format
  = Cmark
  | Dhall
  | Json
  | TextHtml
  | Css
  | InMemory
  deriving (Show, Eq, Ord)

data Section a
  =  Section SectionType Format a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
