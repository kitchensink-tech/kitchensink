module KitchenSink.Blog.Assembler.Sections.CommonMark
 where

import qualified Commonmark
import qualified Commonmark.Extensions as CommonmarkE
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

import KitchenSink.Blog.Build.Target
import KitchenSink.Blog.Section
import KitchenSink.Prelude
import qualified KitchenSink.Commonmark.Free as FreeCommonmark
import KitchenSink.Commonmark.BlogHTML

import KitchenSink.Blog.Assembler.Sections.PreRendered (PreRenderedHtml(..))

dumpCMark :: Section [Text] -> Assembler (Section (FreeCommonmark.Block ()))
dumpCMark (Section ty Cmark lines) = do
   let customSyntax = Commonmark.defaultSyntaxSpec
   res <- Commonmark.commonmarkWith customSyntax "inline" (Text.unlines lines)
   case res of
     Left err -> Assembler $ Left (CommonMarkRenderingError err)
     Right (blk :: FreeCommonmark.Block ()) -> Assembler $ Right $ Section ty InMemory  blk
dumpCMark (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

parseCMark :: Section [Text] -> Assembler (Section Html)
parseCMark (Section ty Cmark lines) = do
   let customSyntax =
           mconcat
           [ CommonmarkE.fencedDivSpec
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

renderCMark :: Section [Text] -> Assembler (Section PreRenderedHtml)
renderCMark sec =
    fmap (fmap r) $ parseCMark sec
  where
    r (html :: Html) =
        PreRenderedHtml
        $ LText.toStrict
        $ renderHtml html
