module KitchenSink.Layout.Base where

import KitchenSink.Core.Build.Site (Site)
import KitchenSink.Core.Build.Target (ExecRoot, OutputPrefix, SourceLocation, Target)
import KitchenSink.Core.Section (ExtraSectionType)
import KitchenSink.Prelude

data Layout ext meta summary = Layout
    { siteTargets :: ExecRoot -> OutputPrefix -> meta -> Site ext -> [Target ext summary]
    , siteDiagnostics :: Site ext -> [Diagnostic]
    -- ^ problems found in the sources that 'siteTargets' works around (an
    -- unknown layout falling back to the default one, a generator section that
    -- cannot be read, ...), for the engine to report at load time
    , extraSectiontypes :: [ExtraSectionType ext]
    }

data Severity
    = -- | the site is still built as intended, modulo a fallback
      Warning
    | -- | part of the site could not be built
      Failure
    deriving (Show, Eq)

data Diagnostic = Diagnostic
    { severity :: Severity
    , source :: SourceLocation
    , message :: Text
    }
    deriving (Show, Eq)
