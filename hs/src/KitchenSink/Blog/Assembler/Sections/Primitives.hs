module KitchenSink.Blog.Assembler.Sections.Primitives
 ( getSection
 , getSections
 , lookupSection
 ) where

import qualified Data.List as List

import KitchenSink.Blog.Build.Site
import KitchenSink.Blog.Build.Target
import KitchenSink.Blog.Section
import KitchenSink.Blog.Prelude

getSection :: Article a -> SectionType -> Assembler (Section a)
getSection (Article _ xs) tyA =
    Assembler
    $ maybe (Left $ SectionNotFound tyA) Right
    $ List.find f xs
  where
    f (Section tyB  _ _) = tyB == tyA

getSections :: Article a -> SectionType -> Assembler [Section a]
getSections (Article _ xs) tyA =
    pure
    $ List.filter f xs
  where
    f (Section tyB  _ _) = tyB == tyA

lookupSection :: Article a -> SectionType -> Assembler (Maybe (Section a))
lookupSection (Article _ xs) tyA =
    pure $ List.find f xs
  where
    f (Section tyB  _ _) = tyB == tyA

