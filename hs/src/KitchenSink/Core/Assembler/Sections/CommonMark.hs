module KitchenSink.Core.Assembler.Sections.CommonMark
where

import Commonmark qualified
import Commonmark.Extensions qualified as CommonmarkE
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Typeable (Typeable)
import KitchenSink.Commonmark.HashTag qualified as CommonmarkE

import KitchenSink.Commonmark.BlogHTML
import KitchenSink.Commonmark.Free qualified as FreeCommonmark
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

import KitchenSink.Core.Assembler.Sections.PreRendered (PreRenderedHtml (..))

dumpCMark :: Section ext [Text] -> Assembler ext (Section ext (FreeCommonmark.Block))
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
        Left err ->
            Assembler $ Left (CommonMarkRenderingError err)
        Right (blk :: FreeCommonmark.Block) ->
            Assembler $ Right $ Section ty InMemory blk
dumpCMark (Section _ fmt _) =
    Assembler $ Left (UnsupportedConversionFormat fmt)

parseCMark :: Section ext [Text] -> Assembler ext (Section ext Html)
parseCMark = parseCMarkWith PlainExternalLinks

{- | Like 'parseCMark', with links to other hosts rendered according to the
given policy.
-}
parseCMarkWith :: ExternalLinkPolicy -> Section ext [Text] -> Assembler ext (Section ext Html)
parseCMarkWith PlainExternalLinks sec = parseCMarkAs @Html sec
parseCMarkWith NoReferrerExternalLinks sec = fmap (fmap coerce) (parseCMarkAs @NoReferrerHtml sec)

parseCMarkAs ::
    forall html ext.
    ( Commonmark.IsBlock html html
    , Commonmark.IsInline html
    , Commonmark.ToPlainText html
    , CommonmarkE.HasHashTag html
    , CommonmarkE.HasDiv html
    , CommonmarkE.HasSpan html
    , CommonmarkE.HasEmoji html
    , CommonmarkE.HasQuoted html
    , Typeable html
    ) =>
    Section ext [Text] ->
    Assembler ext (Section ext html)
parseCMarkAs (Section ty Cmark lines) = do
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
        Right (html :: html) -> Assembler $ Right $ Section ty InMemory html
parseCMarkAs (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

renderCMark :: Section ext [Text] -> Assembler ext (Section ext PreRenderedHtml)
renderCMark = renderCMarkWith PlainExternalLinks

renderCMarkWith :: ExternalLinkPolicy -> Section ext [Text] -> Assembler ext (Section ext PreRenderedHtml)
renderCMarkWith policy sec =
    fmap (fmap r) $ parseCMarkWith policy sec
  where
    r (html :: Html) =
        PreRenderedHtml
            $ LText.toStrict
            $ renderHtml html
