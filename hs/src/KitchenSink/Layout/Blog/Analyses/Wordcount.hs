module KitchenSink.Layout.Blog.Analyses.Wordcount where

import qualified Commonmark
import Data.Text (Text)
import qualified Data.Text as Text

import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections

wordcount :: Text -> Int
wordcount = length . Text.words

contentWordCount :: [Section [Text]] -> Int
contentWordCount xs = sum $ fmap go xs
  where
    go :: Section [Text] -> Int
    go sec =
        case runAssembler $ parseCMark sec of
          Left _ -> sum $ [ wordcount w | w <- extract sec ]
          Right html -> wordcount $ Commonmark.toPlainText $ extract html

