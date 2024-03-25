module KitchenSink.Core.Section.Parser (
    module KitchenSink.Core.Section.Base,
    extract,
    extract',
    section,
    ExtraSectionType (..),
    Parser,
)
where

import Data.Char (isAlphaNum)
import Data.Foldable (asum)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, string)

import KitchenSink.Core.Section.Base
import KitchenSink.Prelude

extract' :: (Coercible a b) => Section ext a -> b
extract' (Section _ _ a) = coerce a

extract :: Section ext a -> a
extract (Section _ _ a) = a

type Parser = Parsec Void Text

data ExtraSectionType userdef
    = ExtraSectionType
    { key :: Text
    , val :: userdef
    }

headers :: [ExtraSectionType ext] -> Parser (SectionType ext, Format)
headers extras =
    asum (basics <> complicated <> dangerous <> extensions)
  where
    dotFormat :: Parser Format
    dotFormat = string "." *> format

    nsKeyVal :: forall value. Text -> Text -> value -> Parser value
    nsKeyVal ns k v = string ("=" <> ns <> ":" <> k) *> pure v

    base k v = (,) <$> nsKeyVal "base" k v <*> dotFormat
    gen k v = (,) <$> nsKeyVal "generator" k v <*> dotFormat
    ext k v = (,) <$> nsKeyVal "ext" k v <*> dotFormat

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

    -- for dataset the format is before the dataset
    complicated =
        [ adaptDataset <$> (nsKeyVal "base" "dataset" Dataset) <*> dotFormat <*> (space *> kebabString)
        ]
      where
        adaptDataset :: (Name -> SectionType ext) -> Format -> Text -> (SectionType ext, Format)
        adaptDataset f fmt name = (f name, fmt)

    dangerous =
        [ gen "cmd" GeneratorInstructions
        ]

    extensions =
        [ext k (Extension v) | ExtraSectionType k v <- extras]

format :: Parser Format
format = cmark <|> json <|> css <|> csv <|> dhall
  where
    cmark = string "cmark" *> pure Cmark
    json = string "json" *> pure Json
    css = string "css" *> pure Css
    csv = string "csv" *> pure Csv
    dhall = string "dhall" *> pure Dhall

section :: forall ext. [ExtraSectionType ext] -> Parser (Section ext [Text])
section extras = f <$> (hdrs <?> "section-headers") <*> body
  where
    f (ty, fmt) b = Section ty fmt b

    hdrs :: Parser (SectionType ext, Format)
    hdrs = headers extras

    body :: Parser [Text]
    body = many (try emptyline <|> try contentLine)

    emptyline :: Parser Text
    emptyline = newline >> notFollowedBy hdrs >> pure ""

    contentLine :: Parser Text
    contentLine = takeWhile1P Nothing ((/=) '\n') <* newline

kebabString :: Parser Text
kebabString = takeWhile1P (Just "kebab-cased-char") isKebabChar

isKebabChar :: Token Text -> Bool
isKebabChar '-' = True
isKebabChar x = isAlphaNum x
