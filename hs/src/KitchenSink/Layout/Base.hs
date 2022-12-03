
module KitchenSink.Layout.Base where

import KitchenSink.Core.Build.Target (OutputPrefix, Target)
import KitchenSink.Core.Build.Site (Site)

data Layout meta summary = Layout {
    siteTargets :: OutputPrefix -> meta -> Site -> [Target summary]
  }
