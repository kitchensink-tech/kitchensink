
module KitchenSink.Layout.Blog.ArticleTypes where

import Data.Text (Text)

import KitchenSink.Core.Build.Site ()
import KitchenSink.Core.Assembler (runAssembler)
import KitchenSink.Core.Section hiding (Section)
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections
import KitchenSink.Layout.Blog.Extensions (Article,AssemblerError)

data ArticleLayout
 = UnknownLayout Text
 | ErrorLayout AssemblerError
 | PublishedArticle
 | UpcomingArticle
 | ArchivedArticle
 | IndexPage
 | TopicListingPage
 | SinglePageApp
 | ImageGallery
 | VariousListing
 deriving (Show, Eq)

layoutNameFor :: Article [Text] -> ArticleLayout
layoutNameFor art = 
  case runAssembler (json @() @BuildInfoData art BuildInfo) of
     Left err -> ErrorLayout err 
     Right s -> let binfo = extract s
                in case publicationStatus binfo of
                     Nothing     -> effectiveLayout Public (layout binfo)
                     Just status -> effectiveLayout status (layout binfo)

effectiveLayout :: PublicationStatus -> Text -> ArticleLayout
effectiveLayout Public "article" = PublishedArticle
effectiveLayout Public "index" = IndexPage
effectiveLayout Public "topics" = TopicListingPage
effectiveLayout Public "application" = SinglePageApp
effectiveLayout Public "gallery" = ImageGallery
effectiveLayout Public "listing" = VariousListing
effectiveLayout Public t = UnknownLayout t
effectiveLayout Upcoming "article" = UpcomingArticle
effectiveLayout Upcoming "application" = SinglePageApp
effectiveLayout Upcoming "gallery" = ImageGallery
effectiveLayout Upcoming "listing" = VariousListing
effectiveLayout Upcoming t = UnknownLayout t
effectiveLayout Archived "article" = ArchivedArticle
effectiveLayout Archived t = UnknownLayout t
