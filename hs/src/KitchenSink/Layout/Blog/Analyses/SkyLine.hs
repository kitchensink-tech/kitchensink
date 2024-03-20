{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module KitchenSink.Layout.Blog.Analyses.SkyLine (
    SkyLine (..),
    SkyLineItem (..),
    sectionSkyLine,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Prelude (succ, (-))

import KitchenSink.Commonmark.Free as CMark
import KitchenSink.Prelude

newtype SkyLine = SkyLine {skylineItems :: [SkyLineItem]}
    deriving (Show)
    deriving (Generic)
    deriving (Semigroup) via [SkyLineItem]
    deriving (Monoid) via [SkyLineItem]
instance ToJSON SkyLine
instance FromJSON SkyLine

data SkyLineItem
    = HeaderMark Text HeadingLevels
    | ImageMark Text Text
    | TextualMark TextWeight HeadingLevels
    deriving (Show)
    deriving (Generic)
instance ToJSON SkyLineItem
instance FromJSON SkyLineItem

type TextWeight = Int -- todo newtype over Sum Int
type HeadingLevels = [Int] -- todo newtype over nonempty list

sectionSkyLine :: (CMark.Block a) -> SkyLine
sectionSkyLine s =
    let u = s : blockUniplate s
     in SkyLine $ List.reverse $ snd $ foldSkylineBlocks [c | b <- u, c <- blockChunks b]
  where
    foldSkylineBlocks :: [CMark.BlockChunk a] -> (HeadingLevels, [SkyLineItem])
    foldSkylineBlocks = List.foldl' foldOneBlock ([0], [])

    foldOneBlock :: (HeadingLevels, [SkyLineItem]) -> CMark.BlockChunk a -> (HeadingLevels, [SkyLineItem])
    foldOneBlock (hdrs, xs) b =
        let hdrs' = case b of
                Heading n _ -> updateHeaders n hdrs
                _ -> hdrs
            item = case b of
                Heading _ il ->
                    HeaderMark (mconcat $ fmap flattenText $ inlineChunks $ il) hdrs'
                x ->
                    let directInlines = blockChunkInlines x
                        childrenInlines = List.concatMap blockInlines (blockChunkBlocks x)
                        allInlines = childrenInlines <> directInlines
                     in TextualMark (List.sum $ fmap go allInlines) hdrs'
         in (hdrs', item : xs)

    updateHeaders :: Int -> [Int] -> [Int]
    updateHeaders level counters =
        case List.drop (List.length counters - level) counters of
            [] -> 1 : counters
            cnt : levelcounters -> succ cnt : levelcounters

    go il = List.sum [f c | c <- inlineChunks il]
    f x = case x of
        Str t -> wc t
        Link _ t _ -> wc t
        Code t -> wc t
        Entity _ -> 1
        EscapedChar _ -> 1
        _ -> 0
    flattenText x = case x of
        Str t -> t
        Link _ t _ -> t
        Code t -> t
        _ -> mempty

    wc = List.length . List.filter (\x -> not $ List.elem x ["", ":"]) . Text.words
