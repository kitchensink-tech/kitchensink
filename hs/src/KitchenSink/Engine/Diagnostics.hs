{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Diagnostics (
    reportDiagnostics,
    hasFailures,
) where

import Data.Foldable (any)
import Data.Text qualified as Text
import System.IO (hPutStrLn, stderr)

import KitchenSink.Core.Build.Target (SourceLocation (..))
import KitchenSink.Layout.Base (Diagnostic (..), Severity (..))
import KitchenSink.Prelude

-- | One line per diagnostic on stderr, @path: warning: message@.
reportDiagnostics :: [Diagnostic] -> IO ()
reportDiagnostics = traverse_ (hPutStrLn stderr . render)
  where
    render :: Diagnostic -> String
    render d =
        mconcat
            [ case d.source of FileSource path -> path
            , ": "
            , case d.severity of
                Warning -> "warning"
                Failure -> "error"
            , ": "
            , Text.unpack d.message
            ]

hasFailures :: [Diagnostic] -> Bool
hasFailures = any ((== Failure) . (.severity))
