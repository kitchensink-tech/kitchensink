{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module KitchenSink.Blog (siteTargets) where

import GHC.Err (error)
import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Lucid as Lucid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.List as List
import System.FilePath.Posix (takeFileName)
import qualified Data.ByteString.Lazy as LByteString

import KitchenSink.Blog.Target
import KitchenSink.Blog.Generator
import KitchenSink.Blog.Section
import KitchenSink.Blog.Site
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Basics
import KitchenSink.Blog.Layout
import KitchenSink.Blog.AssembleSections
import KitchenSink.Blog.Advanced

imageTargets :: OutputPrefix -> Site -> [Target]
imageTargets prefix site =
  [ Target (destImage prefix loc) (copyFrom loc) | Sourced loc _ <- images site ]

dotimageTargets :: OutputPrefix -> Tracer -> Site -> [Target]
dotimageTargets prefix trace site =
  [ Target (destGenImage prefix loc GenPngFile) (execCmd trace "dot" ["-Tpng", "-o", "/dev/stdout", path] "") | Sourced loc@(FileSource path) _ <- dotSourceFiles site ]

videoTargets :: OutputPrefix -> Site -> [Target]
videoTargets prefix site =
  [ Target (destVideoFile prefix loc) (copyFrom loc) | Sourced loc _ <- videoFiles site ]

rawTargets :: OutputPrefix -> Site -> [Target]
rawTargets prefix site =
  [ Target (destRawFile prefix loc) (copyFrom loc) | Sourced loc _ <- rawFiles site ]

cssTargets :: OutputPrefix -> Site -> [Target]
cssTargets prefix site =
  [ Target (destCssFile prefix loc) (copyFrom loc) | Sourced loc _ <- cssFiles site ]

jsTargets :: OutputPrefix -> Site -> [Target]
jsTargets prefix site =
  [ Target (destJsFile prefix loc) (copyFrom loc) | Sourced loc _ <- jsFiles site ]

jsonDataTarget :: ToJSON a => OutputPrefix -> a -> FilePath -> Target
jsonDataTarget prefix v loc =
  Target (destJsonDataFile prefix loc) (ProduceGenerator $ Generator $ pure $ Right $ LByteString.toStrict $ encode v)

siteTargets :: OutputPrefix -> Tracer -> MetaExtraData -> Site -> [Target]
siteTargets prefix tracer extra site = allTargets
  where
    allTargets = mconcat
      [ embeddedGeneratorTargets
      , fmap fst articleTargets
      , imageTargets prefix site
      , dotimageTargets prefix tracer site
      , videoTargets prefix site
      , rawTargets prefix site
      , cssTargets prefix site
      , jsTargets prefix site
      , tagIndexesTargets (lookupSpecialArticle "tags.cmark" site)
      , jsonDataTargets
      ]

    jsonDataTargets :: [Target]
    jsonDataTargets =
      [ jsonDataTarget prefix (fmap (destinationUrl . destination) allTargets) "paths.json"
      , jsonDataTarget prefix (filecounts site) "filecounts.json"
      , jsonDataTarget prefix (topicsgraph stats) "topicsgraph.json"
      ] <> [ jsonDataTarget prefix (analyzeArticle art) (p <> ".json") | (Sourced (FileSource p) art) <- articles site
      ]

    articleTarget :: Sourced (Article [Text]) -> Target
    articleTarget (Sourced loc@(FileSource path) art) =
      let u = destHtml prefix loc
          j = destJsonDataFile prefix (path <> ".json") -- todo:unify
      in Target u (ProduceAssembler $ layoutFor u j art)

    articleTargets :: [(Target, Article [Text])]
    articleTargets =
      [ (articleTarget srca, obj srca)
      | srca <- articles site
      , isConcreteTarget $ obj srca
      ]

    -- TODO: raise errors here
    embeddedGeneratorTargets :: [Target]
    embeddedGeneratorTargets =
      [ tgt
      | Sourced loc art <- articles site
      , tgt <- getTargets loc art
      ]
      where
        getTargets :: SourceLocation -> Article [Text] -> [Target]
        getTargets loc art = either (error . show) (fmap (genTarget loc)) . runAssembler $ (f art)

        genTarget :: SourceLocation -> GeneratorInstructionsData -> Target
        genTarget loc g = let rule = execCmd tracer (Text.unpack $ cmd g) (fmap Text.unpack $ args g) (maybe "" Text.encodeUtf8 $ stdin g)
                      in Target (destGenArbitrary prefix loc g) rule

        f :: Article [Text] -> Assembler [GeneratorInstructionsData]
        f art = getSections art GeneratorInstructions
            >>= traverse (fmap extract . jsonSection)

    tagIndexesTargets :: Maybe (Article [Text]) -> [ Target ]
    tagIndexesTargets Nothing = []
    tagIndexesTargets (Just art) = [ let u = destTag prefix tag in Target u (ProduceAssembler $ tagsLayout tag u u art) | tag <- Map.keys $ byTopic stats ]

    layoutFor :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    layoutFor dloc jsondloc art =
      let go = fromMaybe defaultLayout
             $ flip List.lookup layoutMap
             $ layoutNameFor art
      in go dloc jsondloc art

    layoutMap :: [(ArticleLayout, DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text)]
    layoutMap =
      [ (SinglePageApp, spaLayout)
      , (VariousListing, variousListingLayout)
      , (ImageGallery, imageGalleryLayout)
      , (UpcomingArticle, upcomingArticleLayout)
      , (PublishedArticle, articleLayout)
      , (IndexPage, indexLayout)
      ]

    stats :: TopicStats
    stats = buildTopicStats (articles site) articleTarget 

    indexLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    indexLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc
                    $ mconcat
                      [ assembleStyle
                      ]
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleMain
                        , const $ pure $ latestArticleLink articleTargets
                        , const $ pure $ siteGraphEchartZone
                        , const $ pure $ mainArticleLinks articleTargets
                        , const $ pure $ topicsListings stats
                        ]
                      ]
                  ]

    upcomingArticleLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    upcomingArticleLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleHeader prefix stats
                        , assembleUpcomingMain
                        , assembleFooter
                        ]
                      ]
                  ]

    articleLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    articleLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleHeader prefix stats
                        , assembleMain
                        , assembleFooter
                        ]
                      ]
                  ]

    spaLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    spaLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ id_ "spa", class_ "application"]) mempty
                      , wrap (div_ [ class_ "help"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleMain
                        ]
                      ]
                  ]

    imageGalleryLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    imageGalleryLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ id_ "gallery", class_ "photos"])
                      $ mconcat
                        [ assembleMain
                        ]
                      ]
                  ]

    variousListingLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    variousListingLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (div_ [ id_ "listing" ])
                      $ mconcat
                        [ assembleMain
                        ]
                      ]
                  ]


    tagsLayout :: Tag -> DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    tagsLayout tag dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleTopicListing prefix stats tag
                        ]
                      ]
                  ]

    -- TODO: add warning on default layout
    defaultLayout :: DestinationLocation -> DestinationLocation -> Article [Text] -> Assembler LText.Text
    defaultLayout dloc jsondloc =
      htmldoc
        $ mconcat [ htmlhead extra dloc jsondloc assembleStyle
                  , htmlbody 
                    $ mconcat
                      [ wrap (nav_ [ id_ "site-navigation", class_ "nav"])
                      $ mconcat
                        [ const $ pure $ homeLink
                        , const $ pure $ searchBox
                        ]
                      , wrap (div_ [ class_ "main"])
                      $ wrap article_ 
                      $ mconcat
                        [ assembleHeader prefix stats
                        , assembleDefaultLayoutWarning
                        , assembleMain
                        , assembleFooter
                        ]
                      ]
                  ]

lookupSpecialArticle :: Text -> Site -> Maybe (Article [Text])
lookupSpecialArticle name site = obj <$> List.find f (articles site)
  where
    f :: Sourced a -> Bool
    f (Sourced (FileSource path) _) = takeFileName path == Text.unpack name
