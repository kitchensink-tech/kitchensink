module KitchenSink.Core.Section.Parser
  ( module KitchenSink.Core.Section.Base
  , extract
  , extract'
  , section
  , sectionType
  , ExtraSectionType(..)
  , Parser
  )
where

import Data.Foldable (asum)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char (string, newline)
import Data.Void (Void)

import KitchenSink.Prelude
import KitchenSink.Core.Section.Base

extract' :: Coercible a b => Section ext a -> b
extract' (Section _ _ a) = coerce a

extract :: Section ext a -> a
extract (Section _ _ a) = a

type Parser = Parsec Void Text

data ExtraSectionType userdef
  = ExtraSectionType
  { key :: Text
  , val :: userdef
  }

sectionType :: [ExtraSectionType ext] -> Parser (SectionType ext)
sectionType extras =
    asum (basics <> dangerous <> extensions)
  where
    mkSectionType ns k v = string (ns <> ":" <> k) *> pure v
    base = mkSectionType "base"
    gen = mkSectionType "generator"
    ext = mkSectionType "ext"

    basics = 
      [ base "build-info" BuildInfo
      , base "preamble" Preamble
      , base "topic" Topic
      , base "main-content" MainContent
      , base "summary" Summary
      , base "main-css" MainCss
      , base "taken-off" TakenOff
      , base "social" Social
      , base "glossary" Glossary
      ]

    dangerous =
      [ gen "cmd" GeneratorInstructions
      ]

    extensions =
      [ ext k (Extension v) | ExtraSectionType k v <- extras ]

format :: Parser Format
format = cmark <|> json <|> css <|> dhall
  where
    cmark = string "cmark" *> pure Cmark
    json = string "json" *> pure Json
    css = string "css" *> pure Css
    dhall = string "dhall" *> pure Dhall

section :: forall ext. [ExtraSectionType ext] -> Parser (Section ext [Text])
section extras = f <$> (headers <?> "section-headers") <*> body
  where
    f (ty, fmt) b = Section ty fmt b

    headers :: Parser (SectionType ext, Format)
    headers = (,) <$> (string "=" *> sectionType extras) <*> (string "." *> format)

    body :: Parser [Text]
    body = many (try emptyline <|> try contentLine)

    emptyline :: Parser Text
    emptyline = newline >> notFollowedBy headers >> pure ""

    contentLine :: Parser Text
    contentLine = takeWhile1P Nothing ((/=) '\n') <* newline
