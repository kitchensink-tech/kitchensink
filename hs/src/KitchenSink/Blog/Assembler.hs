{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module KitchenSink.Blog.Assembler (Assembler(..), AssemblerError(..))
where

import qualified Commonmark

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Section

data AssemblerError
  = SectionNotFound SectionType
  | UnsupportedConversionFormat Format
  | CommonMarkRenderingError Commonmark.ParseError
  | JsonDecodingError String
  deriving (Show, Eq)
instance Exception AssemblerError

newtype Assembler a = Assembler { runAssembler :: Either AssemblerError a }
  deriving (Show, Functor, Applicative, Monad)

instance Semigroup a => Semigroup (Assembler a) where
  a1 <> a2 = (<>) <$> a1 <*> a2

instance Monoid a => Monoid (Assembler a) where
  mempty = pure mempty

