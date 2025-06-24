module KitchenSink.Layout.Blog.Analyses.TextRender where

import Commonmark qualified
import Data.Text qualified as Text

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
        case runAssembler $ parseCMark sec of
            Left _ -> "\n"
            Right html -> Commonmark.toPlainText $ extract html
