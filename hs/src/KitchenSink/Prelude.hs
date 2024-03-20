module KitchenSink.Prelude (
    FilePath,
    hush,
    module Control.Applicative,
    module Control.Monad,
    module Data.Bool,
    module Data.Coerce,
    module Data.Eq,
    module Data.Either,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.Int,
    module Data.Traversable,
    module Data.Tuple,
    module Data.List,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.Semigroup,
    module Data.String,
    module System.IO,
    module Text.Show,
    module GHC.Exception,
    module GHC.IO,
    module Prelude,
) where

import Control.Applicative (Applicative, pure, (*>), (<*), (<*>), (<|>))
import Control.Monad (Functor, Monad, fmap, join, void, (=<<), (>>), (>>=))
import Data.Bool (Bool (..), not, otherwise)
import Data.Coerce (Coercible, coerce)
import Data.Either (Either (..), either)
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (Foldable (..), traverse_)
import Data.Function (const, flip, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (length)
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
import Data.Ord (Ord, compare)
import Data.Semigroup (Semigroup (..), (<>))
import Data.Traversable (Traversable (..), traverse)
import Data.Tuple (fst, snd, uncurry)
import Text.Show (Show, show)
import Prelude (seq)

import Data.String (String)
import GHC.Exception (Exception)
import GHC.IO (IO)
import System.IO (print, putStrLn)

type FilePath = String

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
