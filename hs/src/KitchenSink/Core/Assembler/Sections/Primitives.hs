module KitchenSink.Core.Assembler.Sections.Primitives
 ( getSection
 , getSections
 , lookupSection
 ) where

import qualified Data.List as List

import KitchenSink.Core.Build.Site
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

getSection :: (Eq ext) => Article ext a -> SectionType ext -> Assembler ext (Section ext a)
getSection (Article _ xs) tyA =
    Assembler
    $ maybe (Left $ SectionNotFound tyA) Right
    $ List.find f xs
  where
    f (Section tyB  _ _) = tyB == tyA

getSections :: (Eq ext) => Article ext a -> SectionType ext -> Assembler ext [Section ext a]
getSections (Article _ xs) tyA =
    pure
    $ List.filter f xs
  where
    f (Section tyB  _ _) = tyB == tyA

lookupSection :: (Eq ext) => Article ext a -> SectionType ext -> Assembler ext (Maybe (Section ext a))
lookupSection (Article _ xs) tyA =
    pure $ List.find f xs
  where
    f (Section tyB  _ _) = tyB == tyA

