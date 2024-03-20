module KitchenSink.Layout.Blog.Analyses.Wordcount where

import Commonmark qualified
import Data.Text (Text)
import Data.Text qualified as Text

import KitchenSink.Core.Assembler.Sections
import KitchenSink.Core.Build.Target (runAssembler)
import KitchenSink.Core.Section (extract)
import KitchenSink.Layout.Blog.Extensions (Section)
import KitchenSink.Prelude

wordcount :: Text -> Int
wordcount = length . Text.words

contentWordCount :: [Section [Text]] -> Int
contentWordCount xs = sum $ fmap go xs
  where
    go :: Section [Text] -> Int
    go sec =
        case runAssembler $ parseCMark sec of
            Left _ -> sum $ [wordcount w | w <- extract sec]
            Right html -> wordcount $ Commonmark.toPlainText $ extract html
