module KitchenSink.Layout.Blog.SpecialArticles (SpecialArticle (..), lookupSpecialArticle, lookupSpecialArticleSource) where

import Data.List qualified as List
import Data.Text qualified as Text
import System.FilePath.Posix (takeFileName)

import KitchenSink.Core.Build.Site (articles)
import KitchenSink.Core.Build.Target (SourceLocation (..), Sourced (..))
import KitchenSink.Layout.Blog.Extensions (Article, Site)
import KitchenSink.Prelude

{- | List of special articles that warrant a specific handling such as:
* not being an articleTarget in the list of targets
* consolidated analysses (such as a list of topics/glossary terms)
TODO: dilute this special handling within layoutNameFor in ArticleTypes and isConcreteTarget in Fragments
-}
data SpecialArticle
    = Topics
    | Glossary
    | HashTagListings

articleName :: SpecialArticle -> Text
articleName a = case a of
    Topics -> "topics.cmark"
    Glossary -> "glossary.cmark"
    HashTagListings -> "hashtags.cmark"

lookupSpecialArticleSource :: SpecialArticle -> Site -> Maybe (Sourced (Article [Text]))
lookupSpecialArticleSource a site = List.find f (articles site)
  where
    name :: Text
    name = articleName a

    f :: Sourced a -> Bool
    f (Sourced (FileSource path) _) = takeFileName path == Text.unpack name

lookupSpecialArticle :: SpecialArticle -> Site -> Maybe (Article [Text])
lookupSpecialArticle a site = obj <$> lookupSpecialArticleSource a site
