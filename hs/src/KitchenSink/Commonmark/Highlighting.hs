module KitchenSink.Commonmark.Highlighting (
    CodeLanguage (..),
    Code (..),
    highlightCode,
) where

import Data.ByteString.Lazy qualified as LByteString
import Skylighting
import Text.Blaze.Html qualified as Blaze
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import Data.Text (Text)
import KitchenSink.Prelude

newtype CodeLanguage = CodeLanguage Text
newtype Code = Code Text

highlightCode :: CodeLanguage -> Code -> Maybe LByteString.ByteString
highlightCode fmt code = do
    s <- syntaxByName defaultSyntaxMap (coerce fmt)
    either (const Nothing) (Just . render)
        $ tokenize (TokenizerConfig defaultSyntaxMap False) s (coerce code)
  where
    render tkns =
        renderMarkup
            $ Blaze.toHtml
            $ formatHtmlBlock defaultFormatOpts tkns
