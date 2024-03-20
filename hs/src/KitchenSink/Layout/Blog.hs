module KitchenSink.Layout.Blog (
    module KitchenSink.Layout.Blog.Metadata,
    module KitchenSink.Layout.Base,
    layout,
) where

import KitchenSink.Layout.Base (Layout (..))
import KitchenSink.Layout.Blog.Metadata (MetaData (..))
import KitchenSink.Layout.Blog.Targets qualified as Targets

layout :: Layout () MetaData Targets.TargetSummary
layout =
    Layout
        { siteTargets = Targets.siteTargets
        , extraSectiontypes = []
        }
