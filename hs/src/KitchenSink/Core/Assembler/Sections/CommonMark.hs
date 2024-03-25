module KitchenSink.Core.Assembler.Sections.CommonMark
where

import Commonmark qualified
import Commonmark.Extensions qualified as CommonmarkE
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import KitchenSink.Commonmark.HashTag qualified as CommonmarkE

import KitchenSink.Commonmark.BlogHTML
import KitchenSink.Commonmark.Free qualified as FreeCommonmark
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

import KitchenSink.Core.Assembler.Sections.PreRendered (PreRenderedHtml (..))

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
        Right (blk :: FreeCommonmark.Block ()) -> Assembler $ Right $ Section ty InMemory blk
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
