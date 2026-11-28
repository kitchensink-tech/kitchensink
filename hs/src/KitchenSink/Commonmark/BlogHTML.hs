{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module KitchenSink.Commonmark.BlogHTML where

import Commonmark (HasAttributes (..), IsBlock (..), IsInline, Rangeable, ToPlainText, addAttribute, htmlInline)
import Commonmark qualified
import Commonmark.Extensions (HasDiv, HasEmoji, HasQuoted, HasSpan)
import Data.Foldable (any)
import Data.Ord (min)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import KitchenSink.Commonmark.HashTag (HasHashTag (..))

import KitchenSink.Commonmark.Highlighting
import KitchenSink.Prelude
import Prelude ((+))

newtype Html = Html (Commonmark.Html ())
    deriving (Show)
    deriving (Semigroup)
    deriving (Monoid)
    deriving (HasAttributes) via (Commonmark.Html ())
    deriving (Rangeable) via (Commonmark.Html ())
    deriving (IsInline) via (Commonmark.Html ())
    deriving (HasDiv) via (Commonmark.Html ())
    deriving (HasSpan) via (Commonmark.Html ())
    deriving (HasEmoji) via (Commonmark.Html ())
    deriving (HasQuoted) via (Commonmark.Html ())
    deriving (ToPlainText) via (Commonmark.Html ())

hashtagDestinationUrl :: Text -> Text
hashtagDestinationUrl txt = "/hashtags/" <> txt <> ".html"

instance HasHashTag Html where
    hashtag a =
        Html
            $ addAttribute ("href", hashtagDestinationUrl a)
            $ htmlInline "a"
            $ Just
            $ hashtag a

instance IsBlock Html Html where
    paragraph (Html a) = Html (paragraph a)
    plain (Html a) = Html (plain a)
    thematicBreak = Html thematicBreak
    blockQuote (Html a) = Html (blockQuote a)
    -- Markdown headings start one level below the hand-authored article
    -- <h1> title, so `#` becomes <h2>, `##` becomes <h3>, etc.
    heading n (Html a) = Html (heading (min 6 (n + 1)) a)
    rawBlock f t = Html (rawBlock f t)
    referenceLinkDefinition t kv = Html (referenceLinkDefinition t kv)
    list ty sp xs = Html (list ty sp [coerce x | x <- xs])
    codeBlock lang code =
        case highlightCode (CodeLanguage lang) (Code code) of
            Nothing -> Html (codeBlock lang code)
            Just lbs -> Html $ Commonmark.htmlRaw $ mconcat ["<div class=\"code code--highlighted\">", toStrict $ decodeUtf8 $ lbs, "</div>"]

renderHtml :: Html -> TL.Text
renderHtml = Commonmark.renderHtml . coerce

-- | How the links of a cmark section that point to another host are rendered.
data ExternalLinkPolicy
    = -- | as written (the default)
      PlainExternalLinks
    | -- | with a @rel="noreferrer"@ attribute
      NoReferrerExternalLinks
    deriving (Show, Eq)

{- | An absolute link to another host: @http://@, @https://@ or
protocol-relative @//@. Relative links and in-page anchors are not external.
-}
isExternalLink :: Text -> Bool
isExternalLink target =
    any (`Text.isPrefixOf` Text.toLower target) ["http://", "https://", "//"]

{- | 'Html' whose external links carry @rel="noreferrer"@; everything else is
rendered exactly as 'Html' renders it.

The cmark syntax Kitchen-Sink enables has no way to put attributes on a link,
so a cmark link never carries a @rel@ of its own to merge with. Anchors written
as raw HTML are passed through untouched, @rel@ included.
-}
newtype NoReferrerHtml = NoReferrerHtml Html
    deriving (Show)
    deriving (Semigroup)
    deriving (Monoid)
    deriving (HasAttributes) via Html
    deriving (Rangeable) via Html
    deriving (HasDiv) via Html
    deriving (HasSpan) via Html
    deriving (HasEmoji) via Html
    deriving (HasQuoted) via Html
    deriving (ToPlainText) via Html
    deriving (HasHashTag) via Html

instance IsInline NoReferrerHtml where
    lineBreak = coerce (Commonmark.lineBreak @Html)
    softBreak = coerce (Commonmark.softBreak @Html)
    str = coerce (Commonmark.str @Html)
    entity = coerce (Commonmark.entity @Html)
    escapedChar = coerce (Commonmark.escapedChar @Html)
    emph = coerce (Commonmark.emph @Html)
    strong = coerce (Commonmark.strong @Html)
    link target title (NoReferrerHtml ils)
        | isExternalLink target = NoReferrerHtml $ addAttributes [("rel", "noreferrer")] base
        | otherwise = NoReferrerHtml base
      where
        base = Commonmark.link target title ils
    image = coerce (Commonmark.image @Html)
    code = coerce (Commonmark.code @Html)
    rawInline = coerce (Commonmark.rawInline @Html)

instance IsBlock NoReferrerHtml NoReferrerHtml where
    paragraph = coerce (paragraph @Html @Html)
    plain = coerce (plain @Html @Html)
    thematicBreak = coerce (thematicBreak @Html @Html)
    blockQuote = coerce (blockQuote @Html @Html)
    heading = coerce (heading @Html @Html)
    rawBlock = coerce (rawBlock @Html @Html)
    referenceLinkDefinition = coerce (referenceLinkDefinition @Html @Html)
    list = coerce (list @Html @Html)
    codeBlock = coerce (codeBlock @Html @Html)
