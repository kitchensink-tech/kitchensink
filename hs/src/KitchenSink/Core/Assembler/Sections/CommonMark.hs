module KitchenSink.Core.Assembler.Sections.CommonMark
 where

import qualified Commonmark
import qualified Commonmark.Extensions as CommonmarkE
import qualified KitchenSink.Commonmark.HashTag as CommonmarkE
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude
import qualified KitchenSink.Commonmark.Free as FreeCommonmark
import KitchenSink.Commonmark.BlogHTML

import KitchenSink.Core.Assembler.Sections.PreRendered (PreRenderedHtml(..))

dumpCMark :: Section ext [Text] -> Assembler ext (Section ext (FreeCommonmark.Block ()))
dumpCMark (Section ty Cmark lines) = do
   let customSyntax =
           mconcat
           [ CommonmarkE.hashtagSpec
           , CommonmarkE.fencedDivSpec
           , CommonmarkE.bracketedSpanSpec
           , Commonmark.defaultSyntaxSpec
           ]
   res <- Commonmark.commonmarkWith customSyntax "inline" (Text.unlines lines)
   case res of
     Left err -> Assembler $ Left (CommonMarkRenderingError err)
     Right (blk :: FreeCommonmark.Block ()) -> Assembler $ Right $ Section ty InMemory  blk
dumpCMark (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

parseCMark :: Section ext [Text] -> Assembler ext (Section ext Html)
parseCMark (Section ty Cmark lines) = do
   let customSyntax =
           mconcat
           [ CommonmarkE.hashtagSpec
           , CommonmarkE.fencedDivSpec
           , CommonmarkE.bracketedSpanSpec
           , CommonmarkE.emojiSpec
           , CommonmarkE.smartPunctuationSpec
           , CommonmarkE.autoIdentifiersSpec
           , CommonmarkE.implicitHeadingReferencesSpec
           , Commonmark.defaultSyntaxSpec
           ]
   res <- Commonmark.commonmarkWith customSyntax "inline" (Text.unlines lines)
   case res of
     Left err -> Assembler $ Left (CommonMarkRenderingError err)
     Right (html :: Html) -> Assembler $ Right $ Section ty InMemory html
parseCMark (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

renderCMark :: Section ext [Text] -> Assembler ext (Section ext PreRenderedHtml)
renderCMark sec =
    fmap (fmap r) $ parseCMark sec
  where
    r (html :: Html) =
        PreRenderedHtml
        $ LText.toStrict
        $ renderHtml html
