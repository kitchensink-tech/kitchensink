{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module KitchenSink.Commonmark.BlogHTML where

import qualified Commonmark
import Commonmark.Extensions (HasDiv, HasSpan, HasEmoji, HasQuoted)
import Commonmark (HasAttributes, IsInline, IsBlock(..), Rangeable, ToPlainText)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as TL

import KitchenSink.Blog.Prelude
import KitchenSink.Commonmark.Highlighting

newtype Html = Html (Commonmark.Html ())
  deriving Show
  deriving Semigroup
  deriving Monoid
  deriving HasAttributes via (Commonmark.Html ())
  deriving Rangeable via (Commonmark.Html ())
  deriving IsInline via (Commonmark.Html ())
  deriving HasDiv via (Commonmark.Html ())
  deriving HasSpan via (Commonmark.Html ())
  deriving HasEmoji via (Commonmark.Html ())
  deriving HasQuoted via (Commonmark.Html ())
  deriving ToPlainText via (Commonmark.Html ())

instance IsBlock Html Html  where
  paragraph (Html a) = Html (paragraph a)
  plain (Html a) = Html (plain a)
  thematicBreak = Html thematicBreak
  blockQuote (Html a) = Html (blockQuote a)
  heading n (Html a) = Html (heading n a)
  rawBlock f t = Html (rawBlock f t)
  referenceLinkDefinition t kv = Html (referenceLinkDefinition t kv)
  list ty sp xs = Html (list ty sp [coerce x | x <- xs])
  codeBlock lang code =
    case highlightCode (CodeLanguage lang) (Code code) of
       Nothing  -> Html (codeBlock lang code)
       Just lbs -> Html $ Commonmark.htmlRaw $ mconcat [ "<div class=\"code code--highlighted\">", toStrict $ decodeUtf8 $ lbs, "</div>" ]


renderHtml :: Html -> TL.Text
renderHtml = Commonmark.renderHtml . coerce
