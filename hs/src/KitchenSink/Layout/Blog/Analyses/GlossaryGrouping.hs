
module KitchenSink.Layout.Blog.Analyses.GlossaryGrouping
  ( WholeGlossary(..)
  , buildWholeGlossary
  , allGlossaryTerms
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import KitchenSink.Layout.Blog.Extensions (Target, Article)
import KitchenSink.Core.Build.Target (Sourced(..), runAssembler)
import KitchenSink.Core.Section
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections

data WholeGlossary = WholeGlossary {
    byTerm   :: Map Text [(Target (), Article [Text])]
  }

allGlossaryTerms :: WholeGlossary -> [Text]
allGlossaryTerms = Map.keys . byTerm

buildWholeGlossary :: [Sourced (Article [Text])] -> (Sourced (Article [Text]) -> Target ()) -> WholeGlossary
buildWholeGlossary arts mkTarget =
    WholeGlossary indexByTerm
  where
    indexByTerm :: Map Text [(Target (), Article [Text])]
    indexByTerm = Map.fromListWith (<>)
      $ [ (term t, [(mkTarget s, art)])
      | s@(Sourced _ art) <- arts
      , t <- getGlossaryTerms art
      ]

    getGlossaryTerms :: Article [Text] -> [GlossaryTerm]
    getGlossaryTerms art = either (const []) glossary
      $ runAssembler
      $ fmap extract
      $ json @() @GlossaryData art Glossary
