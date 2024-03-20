{- | Module to simplify types for the layout by pinning the parametrizable
extension to a single known value.
-}
module KitchenSink.Layout.Blog.Extensions where

import KitchenSink.Core.Build.Site qualified as Core
import KitchenSink.Core.Build.Target qualified as Core
import KitchenSink.Core.Section qualified as Core

type Article a = Core.Article () a
type Site = Core.Site ()
type Target a = Core.Target () a
type Assembler a = Core.Assembler () a
type AssemblerError = Core.AssemblerError ()
type Section a = Core.Section () a
type ProductionRule = Core.ProductionRule ()
