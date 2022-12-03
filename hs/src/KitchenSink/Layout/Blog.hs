
module KitchenSink.Layout.Blog
  ( module KitchenSink.Layout.Blog.Metadata
  , module KitchenSink.Layout.Base
  , layout
  ) where

import KitchenSink.Layout.Base (Layout(..))
import qualified KitchenSink.Layout.Blog.Targets as Targets
import KitchenSink.Layout.Blog.Metadata (MetaData(..))

layout :: Layout () MetaData Targets.TargetSummary
layout = Layout
  { siteTargets = Targets.siteTargets
  , extraSectiontypes = []
  }
