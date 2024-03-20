{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitPrelude #-}

module KitchenSink.Commonmark.HashTag (
    HasHashTag (..),
    hashtagSpec,
)
where

import Commonmark.Html
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Syntax
import Commonmark.TokParsers
import Commonmark.Tokens
import Commonmark.Types
import Data.Text (Text)
import Text.Parsec

hashtagSpec ::
    (Monad m, IsBlock il bl, IsInline il, HasHashTag il) =>
    SyntaxSpec m il bl
hashtagSpec =
    mempty
        { syntaxInlineParsers = [withAttributes parseHashTag]
        }

class HasHashTag a where
    hashtag ::
        Text -> -- the hashtag value
        a

instance {-# OVERLAPPABLE #-} HasHashTag (Html a) where
    hashtag kw =
        addAttribute ("class", "hashtag")
            . addAttribute ("data-hashtag", kw)
            $ htmlInline "span"
            $ Just
            $ htmlText
            $ "#" <> kw

instance (HasHashTag i, Monoid i) => HasHashTag (WithSourceMap i) where
    hashtag kw = hashtag kw <$ addName "hashtag"

parseHashTag :: (Monad m, HasHashTag a) => InlineParser m a
parseHashTag = try $ do
    _ <- symbol '#'
    ts <-
        many1 $
            satisfyWord (const True)
                <|> symbol '_'
                <|> symbol '-'
    let kw = untokenize ts
    return $! hashtag kw
