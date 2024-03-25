module KitchenSink.Core.Assembler.Sections.Primitives (
    -- some helper function to locate sections in articles
    SectionPredicate,
    getSection,
    getSections,
    lookupSection,

    -- * prebuild predicates
    isBuildInfo,
    isGlossary,
    isMainContent,
    isMainCss,
    isPreamble,
    isSocial,
    isSummary,
    isTopic,
    isGeneratorInstructions,
    isDataset,
) where

import Data.List qualified as List

import KitchenSink.Core.Build.Site
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

type SectionPredicate ext = forall payload. (Section ext payload -> Bool)

isBuildInfo :: (Eq ext) => SectionPredicate ext
isBuildInfo = (== BuildInfo) . sectionType

isMainContent :: (Eq ext) => SectionPredicate ext
isMainContent = (== MainContent) . sectionType

isMainCss :: (Eq ext) => SectionPredicate ext
isMainCss = (== MainCss) . sectionType

isGeneratorInstructions :: (Eq ext) => SectionPredicate ext
isGeneratorInstructions = (== GeneratorInstructions) . sectionType

isSummary :: (Eq ext) => SectionPredicate ext
isSummary = (== Summary) . sectionType

isTopic :: (Eq ext) => SectionPredicate ext
isTopic = (== Topic) . sectionType

isGlossary :: (Eq ext) => SectionPredicate ext
isGlossary = (== Glossary) . sectionType

isSocial :: (Eq ext) => SectionPredicate ext
isSocial = (== Social) . sectionType

isPreamble :: (Eq ext) => SectionPredicate ext
isPreamble = (== Preamble) . sectionType

isDataset :: SectionPredicate ext
isDataset s = case sectionType s of
    (Dataset _) -> True
    _ -> False

getSection :: (Eq ext) => Article ext a -> SectionPredicate ext -> Assembler ext (Section ext a)
getSection (Article _ xs) f =
    Assembler
        $ maybe (Left $ SectionNotFound) Right
        $ List.find f xs

getSections :: (Eq ext) => Article ext a -> SectionPredicate ext -> Assembler ext [Section ext a]
getSections (Article _ xs) f =
    pure
        $ List.filter f xs

lookupSection :: (Eq ext) => Article ext a -> SectionPredicate ext -> Assembler ext (Maybe (Section ext a))
lookupSection (Article _ xs) f =
    pure $ List.find f xs
