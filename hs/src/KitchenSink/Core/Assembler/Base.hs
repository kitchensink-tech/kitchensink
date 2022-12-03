module KitchenSink.Core.Assembler.Base (Assembler(..), AssemblerError(..)) where

import Data.Typeable
import qualified Commonmark

import KitchenSink.Prelude
import KitchenSink.Core.Section

data AssemblerError ext
  = SectionNotFound (SectionType ext)
  | UnsupportedConversionFormat Format
  | CommonMarkRenderingError Commonmark.ParseError
  | JsonDecodingError String
  deriving (Show, Eq)
instance (Show ext, Typeable ext) => Exception (AssemblerError ext)

newtype Assembler ext a = Assembler { runAssembler :: Either (AssemblerError ext) a }
  deriving (Show, Functor, Applicative, Monad)

instance Semigroup a => Semigroup (Assembler ext a) where
  a1 <> a2 = (<>) <$> a1 <*> a2

instance Monoid a => Monoid (Assembler ext a) where
  mempty = pure mempty


