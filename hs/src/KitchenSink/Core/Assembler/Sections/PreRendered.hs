module KitchenSink.Core.Assembler.Sections.PreRendered (
    PreRenderedHtml (..),
)
where

import Data.Text (Text)

newtype PreRenderedHtml = PreRenderedHtml Text
