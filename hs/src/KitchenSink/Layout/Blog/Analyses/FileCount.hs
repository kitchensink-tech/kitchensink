{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module KitchenSink.Layout.Blog.Analyses.FileCount (
    filecounts,
) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import KitchenSink.Core.Build.Site (articles, audioFiles, cssFiles, dotSourceFiles, images, jsFiles, rawFiles, videoFiles)
import KitchenSink.Layout.Blog.Extensions (Site)
import KitchenSink.Prelude

data FileCount = FileCount
    { srctype :: Text
    , count :: Int
    }
    deriving (Generic, Show)
instance ToJSON FileCount

filecounts :: Site -> [FileCount]
filecounts site =
    [ FileCount "articles" (length $ articles site)
    , FileCount "images" (length $ images site)
    , FileCount "videos" (length $ videoFiles site)
    , FileCount "audios" (length $ audioFiles site)
    , FileCount "raws" (length $ rawFiles site)
    , FileCount "css" (length $ cssFiles site)
    , FileCount "js" (length $ jsFiles site)
    , FileCount "dot" (length $ dotSourceFiles site)
    ]
