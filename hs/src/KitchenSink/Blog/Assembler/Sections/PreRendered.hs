module KitchenSink.Blog.Assembler.Sections.PreRendered
 ( PreRenderedHtml(..)
 )
 where

import Data.Text (Text)

newtype PreRenderedHtml = PreRenderedHtml Text
