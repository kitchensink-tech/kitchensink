module KitchenSink.Layout.Blog.SpecialArticles (SpecialArticle (..), lookupSpecialArticle, lookupSpecialArticleSource) where

import Data.List qualified as List

import KitchenSink.Core.Build.Site (articles)
import KitchenSink.Core.Build.Target (Sourced (..))
import KitchenSink.Layout.Blog.ArticleTypes (ArticleLayout (..), layoutNameFor)
import KitchenSink.Layout.Blog.Extensions (Article, Site)
import KitchenSink.Prelude

{- | List of special articles that warrant a specific handling such as:
* not being an articleTarget in the list of targets (see 'isSpecialLayout')
* consolidated analysses (such as a list of topics/glossary terms)

A special article is the first article whose @layout@ (in its build-info
section) is the matching one, whatever the file is named.
-}
data SpecialArticle
    = Topics
    | Glossary
    | HashTagListings

layoutOf :: SpecialArticle -> ArticleLayout
layoutOf a = case a of
    Topics -> TopicListingTemplate
    Glossary -> GlossaryPage
    HashTagListings -> HashTagListingTemplate

lookupSpecialArticleSource :: SpecialArticle -> Site -> Maybe (Sourced (Article [Text]))
lookupSpecialArticleSource a site = List.find f (articles site)
  where
    f :: Sourced (Article [Text]) -> Bool
    f s = layoutNameFor (obj s) == layoutOf a

lookupSpecialArticle :: SpecialArticle -> Site -> Maybe (Article [Text])
lookupSpecialArticle a site = obj <$> lookupSpecialArticleSource a site
