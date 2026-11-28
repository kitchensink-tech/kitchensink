module KitchenSink.Core.Assembler.Sections (
    module KitchenSink.Core.Assembler.Sections.Primitives,
    module KitchenSink.Core.Assembler.Sections.CommonMark,
    module KitchenSink.Core.Assembler.Sections.Json,
    module KitchenSink.Core.Assembler.Sections.PreRendered,
    renderSection,
    renderSectionWith,
) where

import Data.Text qualified as Text

import KitchenSink.Commonmark.BlogHTML (ExternalLinkPolicy (..))
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

import KitchenSink.Core.Assembler.Sections.CommonMark
import KitchenSink.Core.Assembler.Sections.Json
import KitchenSink.Core.Assembler.Sections.PreRendered
import KitchenSink.Core.Assembler.Sections.Primitives

renderSection :: Section ext [Text] -> Assembler ext (Section ext PreRenderedHtml)
renderSection = renderSectionWith PlainExternalLinks

{- | Like 'renderSection', with the links of a cmark section that point to
another host rendered according to the given policy.
-}
renderSectionWith :: ExternalLinkPolicy -> Section ext [Text] -> Assembler ext (Section ext PreRenderedHtml)
renderSectionWith policy s@(Section _ encoding _) =
    case encoding of
        Cmark -> renderCMarkWith policy s
        TextHtml -> Assembler $ Right $ fmap (coerce Text.unlines) s
        fmt -> Assembler $ Left (UnsupportedConversionFormat fmt)
