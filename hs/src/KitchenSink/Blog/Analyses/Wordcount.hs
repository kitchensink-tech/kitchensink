module KitchenSink.Blog.Analyses.Wordcount where

import qualified Commonmark
import Data.Text (Text)
import qualified Data.Text as Text

import KitchenSink.Blog.Target
import KitchenSink.Blog.Section
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Assembler.Sections

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

