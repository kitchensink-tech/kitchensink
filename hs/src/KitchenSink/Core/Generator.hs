module KitchenSink.Core.Generator (Generator(..), GeneratorError(..)) where

import Data.ByteString (ByteString)

import KitchenSink.Prelude

data GeneratorError
  = GeneratorError String ByteString
  deriving (Show, Eq)
instance Exception GeneratorError

newtype Generator a = Generator { runGenerator :: IO (Either GeneratorError a) }
  deriving (Functor)

instance Show (Generator a) where
  show _ = "Generator"
