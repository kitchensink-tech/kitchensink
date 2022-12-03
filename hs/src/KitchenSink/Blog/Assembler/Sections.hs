module KitchenSink.Blog.Assembler.Sections 
 ( module KitchenSink.Blog.Assembler.Sections.Primitives
 , module KitchenSink.Blog.Assembler.Sections.CommonMark
 , module KitchenSink.Blog.Assembler.Sections.Json
 , module KitchenSink.Blog.Assembler.Sections.PreRendered
 , renderSection
 ) where

import Data.Text (Text)
import qualified Data.Text as Text

import KitchenSink.Blog.Build.Target
import KitchenSink.Blog.Section
import KitchenSink.Prelude

import KitchenSink.Blog.Assembler.Sections.CommonMark
import KitchenSink.Blog.Assembler.Sections.Json
import KitchenSink.Blog.Assembler.Sections.PreRendered
import KitchenSink.Blog.Assembler.Sections.Primitives

renderSection :: Section [Text] -> Assembler (Section PreRenderedHtml)
renderSection s@(Section _ encoding _) =
  case encoding of
    Cmark -> renderCMark s
    TextHtml -> Assembler $ Right $ fmap (coerce Text.unlines) s
    fmt -> Assembler $ Left (UnsupportedConversionFormat fmt)
