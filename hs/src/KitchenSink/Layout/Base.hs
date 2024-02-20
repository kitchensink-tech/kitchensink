
module KitchenSink.Layout.Base where

import KitchenSink.Core.Build.Target (ExecRoot, OutputPrefix, Target)
import KitchenSink.Core.Build.Site (Site)
import KitchenSink.Core.Section (ExtraSectionType)


data Layout ext meta summary = Layout {
    siteTargets :: ExecRoot -> OutputPrefix -> meta -> Site ext -> [Target ext summary]
  , extraSectiontypes :: [ExtraSectionType ext]
  }
