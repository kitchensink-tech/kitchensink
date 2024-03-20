module KitchenSink.Layout.Blog.Analyses.GlossaryGrouping (
    WholeGlossary (..),
    buildWholeGlossary,
    glossaryEntries,
    glossaryTerms,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import KitchenSink.Core.Assembler.Sections
import KitchenSink.Core.Build.Target (Sourced (..), runAssembler)
import KitchenSink.Core.Section
import KitchenSink.Layout.Blog.Extensions (Article, Target)
import KitchenSink.Prelude

type TargetArticle k = (Target (), Article [Text], k)

type Term = Text
type Definition = Text

data WholeGlossary = WholeGlossary
    { byTerm :: Map Term [TargetArticle Definition]
    }

{- | Glossary terms in ascending order.

Implementation relies on Map.keys to return values in ascending order.
-}
glossaryTerms :: WholeGlossary -> [Text]
glossaryTerms = Map.keys . byTerm

{- | Glossary entries in ascending order.

Implementation relies on Map.toAscList to return values in ascending order.
-}
glossaryEntries :: WholeGlossary -> [(Text, [TargetArticle Definition])]
glossaryEntries = Map.toAscList . byTerm

buildWholeGlossary :: [Sourced (Article [Text])] -> (Sourced (Article [Text]) -> Target ()) -> WholeGlossary
buildWholeGlossary arts mkTarget =
    WholeGlossary indexByTerm
  where
    indexByTerm :: Map Text [TargetArticle Definition]
    indexByTerm =
        Map.fromListWith (<>)
            $ [ (term t, [(mkTarget s, art, definition t)])
              | s@(Sourced _ art) <- arts
              , t <- getGlossaryTerms art
              ]

    getGlossaryTerms :: Article [Text] -> [GlossaryTerm]
    getGlossaryTerms art =
        either (const []) glossary
            $ runAssembler
            $ fmap extract
            $ json @() @GlossaryData art Glossary
