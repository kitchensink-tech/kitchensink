{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module KitchenSink.Blog.Generator (Generator(..), GeneratorError(..))
where

import Data.ByteString (ByteString)

import KitchenSink.Blog.Prelude

data GeneratorError
  = GeneratorError String ByteString
  deriving (Show, Eq)
instance Exception GeneratorError

newtype Generator a = Generator { runGenerator :: IO (Either GeneratorError a) }
  deriving (Functor)

instance Show (Generator a) where
  show _ = "Generator"
