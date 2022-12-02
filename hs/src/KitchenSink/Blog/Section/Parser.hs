module KitchenSink.Blog.Section.Parser
  ( module KitchenSink.Blog.Section.Base
  , extract
  , extract'
  , section
  , sectionType
  , Parser
  )
where

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char (string, newline)
import Data.Void (Void)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Section.Base

extract' :: Coercible a b => Section a -> b
extract' (Section _ _ a) = coerce a

extract :: Section a -> a
extract (Section _ _ a) = a

type Parser = Parsec Void Text

sectionType :: Parser SectionType
sectionType =
    build <|> preamble <|> topic <|> mainContent <|> summary <|> takenOff <|> mainCss <|> social <|> generator <|> glossary
  where
    build = string "base:build-info" *> pure BuildInfo
    preamble = string "base:preamble" *> pure Preamble
    topic = string "base:topic" *> pure Topic
    mainContent = string "base:main-content" *> pure MainContent
    summary = string "base:summary" *> pure Summary
    mainCss = string "base:main-css" *> pure MainCss
    takenOff = string "base:taken-off" *> pure TakenOff
    social = string "base:social" *> pure Social
    generator = string "generator:cmd" *> pure GeneratorInstructions
    glossary = string "base:glossary" *> pure Glossary

format :: Parser Format
format = cmark <|> json <|> css <|> dhall
  where
    cmark = string "cmark" *> pure Cmark
    json = string "json" *> pure Json
    css = string "css" *> pure Css
    dhall = string "dhall" *> pure Dhall

section :: Parser (Section [Text])
section = f <$> (headers <?> "section-headers") <*> body
  where
    f (ty, fmt) b = Section ty fmt b

    headers :: Parser (SectionType, Format)
    headers = (,) <$> (string "=" *> sectionType) <*> (string "." *> format)

    body :: Parser [Text]
    body = many (try emptyline <|> try contentLine)

    emptyline :: Parser Text
    emptyline = newline >> notFollowedBy headers >> pure ""

    contentLine :: Parser Text
    contentLine = takeWhile1P Nothing ((/=) '\n') <* newline
