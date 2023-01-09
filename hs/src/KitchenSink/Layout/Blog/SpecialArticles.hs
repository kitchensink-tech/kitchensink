
module KitchenSink.Layout.Blog.SpecialArticles (SpecialArticle(..), lookupSpecialArticle) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath.Posix (takeFileName)

import KitchenSink.Prelude
import KitchenSink.Layout.Blog.Extensions (Article, Site)
import KitchenSink.Core.Build.Site (articles)
import KitchenSink.Core.Build.Target (Sourced(..), SourceLocation(..))

data SpecialArticle
  = Topics

articleName :: SpecialArticle -> Text
articleName a = case a of
  Topics -> "topics.cmark"

lookupSpecialArticle :: SpecialArticle -> Site -> Maybe (Article [Text])
lookupSpecialArticle a site = obj <$> List.find f (articles site)
  where
    name :: Text
    name = articleName a

    f :: Sourced a -> Bool
    f (Sourced (FileSource path) _) = takeFileName path == Text.unpack name
