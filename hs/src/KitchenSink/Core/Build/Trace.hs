module KitchenSink.Core.Build.Trace (Tracer, Trace(..))
  where

import KitchenSink.Prelude

data Trace
  = Executing FilePath [String]
  | Copying FilePath
  | Generating FilePath
  | Assembling FilePath
  deriving Show

type Tracer = Trace -> IO ()
