{-# LANGUAGE OverloadedRecordDot #-}

module KitchenSink.Layout.Blog.Analyses.TextRender where

import Commonmark.Types (Format (..), ListSpacing, ListType (..))
import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Builder.Int qualified as Builder
import Numeric (showHex)

import KitchenSink.Commonmark.Free qualified as FreeCommonmark
import KitchenSink.Core.Assembler.Sections
import KitchenSink.Core.Build.Target (runAssembler)
import KitchenSink.Core.Section (extract)
import KitchenSink.Layout.Blog.Extensions (Article, Section)
import KitchenSink.Prelude

textRender :: Article [Text] -> Text
textRender art =
    case runAssembler (getSections art isMainContent) of
        Left _ -> "\n"
        Right xs -> textRenderSections xs

textRenderSections :: [Section [Text]] -> Text
textRenderSections xs = Text.unlines $ fmap go xs
  where
    go :: Section [Text] -> Text
    go sec =
        case runAssembler $ dumpCMark sec of
            Left _ -> "\n"
            Right html -> makePlainText $ extract html

makePlainText :: (FreeCommonmark.Block ()) -> Text
makePlainText bRoot =
    renderBlock "" bRoot
  where
    renderBlock :: Text -> FreeCommonmark.Block () -> Text
    renderBlock pfx b1 = Text.unlines [pfx <> renderBlockChunk bc | bc <- b1.blockChunks]

    renderBlockChunk :: FreeCommonmark.BlockChunk () -> Text
    renderBlockChunk bc1 =
        case bc1 of
            FreeCommonmark.Paragraph sub ->
                renderInline sub <> "\n"
            FreeCommonmark.Plain sub -> renderInline sub
            FreeCommonmark.ThematicBreak -> "-----------------------------\n"
            FreeCommonmark.BlockQuote sub -> renderBlock "    " sub
            FreeCommonmark.CodeBlock fmt t ->
                mconcat
                    ["```", fmt, "\n", t, "```\n"]
            FreeCommonmark.Heading n sub ->
                mconcat
                    [Text.replicate n "#", " ", renderInline sub, "\n"]
            FreeCommonmark.RawBlock (Format fmt) t ->
                mconcat ["```", fmt, "\n", t, "```\n"]
            FreeCommonmark.List ltype lspacing subz -> Text.unlines (fmap (renderBlock (listPrefix ltype lspacing)) subz)
            FreeCommonmark.NestedDivBlock sub -> renderBlock "    " sub
            FreeCommonmark.ReferenceLinkDefinition _ _ -> "" -- todo
    listPrefix :: ListType -> ListSpacing -> Text
    listPrefix (BulletList _) _ = "- "
    listPrefix (OrderedList n _ _) _ = "- ." <> intToText n <> " "

    intToText :: Int -> Text
    intToText = LText.toStrict . Builder.toLazyText . Builder.decimal

    renderInline :: FreeCommonmark.Inline () -> Text
    renderInline i = mconcat $ fmap renderInlineChunk i.inlineChunks

    renderInlineChunk :: FreeCommonmark.InlineChunk () -> Text
    renderInlineChunk ic = case ic of
        FreeCommonmark.LineBreak -> "--------------------------------------------"
        FreeCommonmark.SoftBreak -> " "
        (FreeCommonmark.Str t) -> t
        (FreeCommonmark.Entity t) -> t
        (FreeCommonmark.EscapedChar c) -> Text.pack ("\\x" <> showHex (Char.ord c) "")
        (FreeCommonmark.Emph sub) -> mconcat ["_", renderInline sub, "_"]
        (FreeCommonmark.Strong sub) -> mconcat ["**", renderInline sub, "**"]
        (FreeCommonmark.Link t0 t1 sub) -> mconcat ["[", renderInline sub, "]", "(", t0, " ", t1, ")"]
        (FreeCommonmark.Image t0 t1 sub) -> mconcat ["![", renderInline sub, "]", "(", t0, " ", t1, ")"]
        (FreeCommonmark.Code t) -> mconcat ["`", t, "`"]
        (FreeCommonmark.HashTag t) -> mconcat ["#", t]
        (FreeCommonmark.RawInline (Format fmt) t) -> mconcat ["```", fmt, "\n", t, "```"]
        (FreeCommonmark.SpanInline sub) -> renderInline sub
