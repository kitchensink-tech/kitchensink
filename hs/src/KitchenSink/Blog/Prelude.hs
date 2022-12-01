
module KitchenSink.Blog.Prelude (
    FilePath
  , hush
  , module Control.Applicative
  , module Control.Monad
  , module Data.Bool
  , module Data.Coerce
  , module Data.Eq
  , module Data.Either
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Int
  , module Data.Traversable
  , module Data.Tuple
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Ord
  , module Data.Semigroup
  , module Data.String
  , module System.IO
  , module Text.Show
  , module GHC.Exception
  , module GHC.IO
  , module Prelude
  ) where

import Text.Show (Show, show)
import Data.Bool (Bool(..), otherwise, not)
import Data.Coerce (Coercible, coerce)
import Data.Eq (Eq, (==), (/=))
import Data.Either (Either(..), either)
import Data.Foldable (Foldable(..), traverse_)
import Data.Function (($), (.), const, flip)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (length)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Data.Ord (Ord, compare)
import Data.Semigroup (Semigroup(..), (<>))
import Data.Traversable (Traversable(..), traverse)
import Data.Tuple (uncurry, fst, snd)
import Control.Monad (Monad, Functor, void, fmap, join, (>>), (>>=), (=<<))
import Control.Applicative (Applicative, pure, (<*>), (*>), (<*), (<|>))
import Prelude (seq)

import Data.String (String)
import GHC.Exception (Exception)
import GHC.IO (IO)
import System.IO (print,putStrLn)

type FilePath = String

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

