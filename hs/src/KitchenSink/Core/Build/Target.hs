module KitchenSink.Core.Build.Target (Target(..), ProductionRule(..), Url, DestinationLocation(..), destinationUrl, SourceLocation(..), Sourced(..), Assembler(..), AssemblerError(..), copyFrom, execCmd, OutputPrefix)
  where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import System.Exit (ExitCode(..))
import System.Process.ByteString(readProcessWithExitCode)

import KitchenSink.Prelude
import KitchenSink.Core.Assembler
import KitchenSink.Core.Generator
import KitchenSink.Core.Build.Trace

type OutputPrefix = FilePath

data ProductionRule ext
  = ProduceAssembler (Assembler ext LText.Text)
  | ProduceGenerator (Tracer -> Generator ByteString.ByteString)
  | ProduceFileCopy (Sourced ())

data Target ext a = Target
  { destination :: DestinationLocation
  , productionRule :: ProductionRule ext
  , summary :: a
  } deriving (Functor)

data SourceLocation = FileSource FilePath
  deriving (Show, Eq, Ord)

data Sourced a = Sourced { location :: SourceLocation, obj :: a }
  deriving (Show, Eq, Ord)

type Url = Text

data DestinationLocation
  = StaticFileDestination Url FilePath
  | VirtualFileDestination Url FilePath
  deriving (Show, Eq, Ord)

destinationUrl :: DestinationLocation -> Url
destinationUrl (StaticFileDestination u _) = u
destinationUrl (VirtualFileDestination u _)  = u

copyFrom :: SourceLocation -> ProductionRule ext
copyFrom path = ProduceFileCopy (Sourced path ())

execCmd :: FilePath -> [String] -> ByteString -> ProductionRule ext
execCmd path args input = ProduceGenerator f
  where
    f trace = Generator $ do
      _ <- trace $ Executing path args
      (code, !out, !err) <- readProcessWithExitCode path args input
      pure $ case code of
        ExitFailure n -> Left (GeneratorError (show n) err)
        _             -> Right out
