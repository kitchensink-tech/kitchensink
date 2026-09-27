module KitchenSink.Layout.Blog.ArticleTypes where

import Data.List qualified as List

import KitchenSink.Core.Assembler (runAssembler)
import KitchenSink.Core.Assembler.Sections
import KitchenSink.Core.Build.Site ()
import KitchenSink.Core.Section hiding (Section)
import KitchenSink.Layout.Blog.Extensions (Article, AssemblerError)
import KitchenSink.Prelude

-- | Internal relay type for picking a given rendering function.
data ArticleLayout
    = UnknownLayout Text
    | ErrorLayout AssemblerError
    | PublishedArticle
    | UpcomingArticle
    | ArchivedArticle
    | IndexPage
    | TopicListingTemplate
    | HashTagListingTemplate
    | GlossaryPage
    | SinglePageApp
    | ImageGallery
    | VariousListing
    | DocumentationPage
    | CorporatePage
    | DynamicPage
    deriving (Show, Eq)

{- | Layouts that never get an ordinary static article target: the
consolidated listings (topics, hashtags, glossary) are built from the whole
site by dedicated targets instead, and 'DynamicPage' is request-time-only
(see "KitchenSink.Engine.Dynamic") so it has no static rendering at all --
'produce' and the dev-server's @\/dev\/targets@ simply skip it.
-}
isSpecialLayout :: ArticleLayout -> Bool
isSpecialLayout l = l `List.elem` [TopicListingTemplate, HashTagListingTemplate, GlossaryPage, DynamicPage]

layoutNameFor :: Article [Text] -> ArticleLayout
layoutNameFor art =
    case runAssembler (json @() @BuildInfoData art isBuildInfo) of
        Left err -> ErrorLayout err
        Right s ->
            let binfo = extract s
             in case publicationStatus binfo of
                    Nothing -> effectiveLayout Public (layout binfo)
                    Just status -> effectiveLayout status (layout binfo)

-- | TODO: find a proper name for the user-input layout-type that we find in the BuildInfo section.
effectiveLayout :: PublicationStatus -> Text -> ArticleLayout
effectiveLayout Public "article" = PublishedArticle
effectiveLayout Public "index" = IndexPage
effectiveLayout Public "topics" = TopicListingTemplate
effectiveLayout Public "hashtags" = HashTagListingTemplate
effectiveLayout Public "glossary" = GlossaryPage
effectiveLayout Public "application" = SinglePageApp
effectiveLayout Public "gallery" = ImageGallery
effectiveLayout Public "listing" = VariousListing
effectiveLayout Public "documentation" = DocumentationPage
effectiveLayout Public "corporate" = CorporatePage
effectiveLayout Public "dynamic" = DynamicPage
effectiveLayout Public t = UnknownLayout t
effectiveLayout Upcoming "article" = UpcomingArticle
effectiveLayout Upcoming "application" = SinglePageApp
effectiveLayout Upcoming "gallery" = ImageGallery
effectiveLayout Upcoming "listing" = VariousListing
effectiveLayout Upcoming "documentation" = DocumentationPage
effectiveLayout Upcoming "corporate" = CorporatePage
effectiveLayout Upcoming t = UnknownLayout t
effectiveLayout Archived "article" = ArchivedArticle
effectiveLayout Archived t = UnknownLayout t
