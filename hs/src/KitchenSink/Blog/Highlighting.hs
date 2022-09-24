
module KitchenSink.Blog.Highlighting
  ( CodeLanguage(..)
  , Code(..)
  , highlightCode
  ) where


import Skylighting
import qualified Text.Blaze.Html as Blaze
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Data.ByteString.Lazy as LByteString

import Data.Text (Text)
import KitchenSink.Blog.Prelude

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
