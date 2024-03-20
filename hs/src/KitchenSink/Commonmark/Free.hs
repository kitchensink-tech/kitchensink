{-# OPTIONS_GHC -fno-warn-orphans #-}

module KitchenSink.Commonmark.Free where

import Commonmark.Extensions (HasDiv (..), HasSpan (..))
import Commonmark.Types
import Control.Applicative (pure)
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Char (Char)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

import KitchenSink.Commonmark.HashTag (HasHashTag (..))

-- INLINES

data InlineChunk a
    = LineBreak
    | SoftBreak
    | Str Text
    | Entity Text
    | EscapedChar Char
    | Emph (Inline a)
    | Strong (Inline a)
    | Link Text Text (Inline a)
    | Image Text Text (Inline a)
    | Code Text
    | HashTag Text
    | RawInline Format Text
    | SpanInline (Inline a)
    deriving (Show, Generic)
instance (ToJSON a) => ToJSON (InlineChunk a)

data Inline a = Inline
    { inlineChunks :: [InlineChunk a]
    , inlineAttributes :: Attributes
    , inlineExtra :: a
    }
    deriving (Show, Generic)
instance (ToJSON a) => ToJSON (Inline a)

instance ToJSON Format where
    toJSON (Format txt) = toJSON txt

inlineUniplate :: Inline a -> [Inline a]
inlineUniplate blk = mconcat [go c | c <- inlineChunks blk]
  where
    only a = [a]
    nil = []
    go bc = case bc of
        Emph il -> only il
        Strong il -> only il
        Link _ _ il -> only il
        Image _ _ il -> only il
        _ -> nil

inlineUniverse :: Inline a -> [Inline a]
inlineUniverse blk = blk : mconcat [inlineUniverse b | b <- inlineUniplate blk]

inline :: a -> InlineChunk a -> Inline a
inline a c = Inline [c] mempty a

inline0 :: InlineChunk () -> Inline ()
inline0 = inline ()

instance Semigroup (Inline ()) where
    Inline c1s a1s _ <> Inline c2s a2s _ = Inline (c1s <> c2s) (a1s <> a2s) ()

instance Monoid (Inline ()) where
    mempty = Inline mempty mempty ()

instance HasAttributes (Inline ()) where
    addAttributes xs (Inline c ys _) = Inline c (xs <> ys) ()

instance HasHashTag (Inline ()) where
    hashtag t = Inline [HashTag t] mempty ()

instance HasSpan (Inline ()) where
    spanWith attrs il = Inline [SpanInline il] attrs ()

instance Rangeable (Inline ()) where
    ranged _ a = a

instance IsInline (Inline ()) where
    lineBreak = inline0 LineBreak
    softBreak = inline0 SoftBreak
    str = inline0 . Str
    entity = inline0 . Entity
    escapedChar = inline0 . EscapedChar
    emph = inline0 . Emph
    strong = inline0 . Strong
    link dst title = inline0 . Link dst title
    image dst title = inline0 . Image dst title
    code = inline0 . Code
    rawInline fmt = inline0 . RawInline fmt

-- BLOCKS

data BlockChunk a
    = Paragraph (Inline a)
    | Plain (Inline a)
    | ThematicBreak
    | BlockQuote (Block a)
    | CodeBlock Text Text
    | Heading Int (Inline a)
    | RawBlock Format Text
    | ReferenceLinkDefinition Text (Text, Text)
    | List ListType ListSpacing [Block a]
    | NestedDivBlock (Block a)
    deriving (Show, Generic)
instance (ToJSON a) => ToJSON (BlockChunk a)

blockChunkInlines :: BlockChunk a -> [Inline a]
blockChunkInlines bc =
    let only a = [a]
        nil = []
     in case bc of
            Paragraph il -> only il
            Plain il -> only il
            Heading _ il -> only il
            _ -> nil

data Block a = Block
    { blockChunks :: [BlockChunk a]
    , blockAttributes :: Attributes
    , blockExtra :: a
    }
    deriving (Show, Generic)
instance (ToJSON a) => ToJSON (Block a)

instance HasDiv (Block ()) where
    div_ bl = Block [NestedDivBlock bl] mempty ()

instance ToJSON EnumeratorType where
    toJSON Decimal = toJSON ("Decimal" :: Text)
    toJSON UpperAlpha = toJSON ("UpperAlpha" :: Text)
    toJSON LowerAlpha = toJSON ("LowerAlpha" :: Text)
    toJSON UpperRoman = toJSON ("UpperRoman" :: Text)
    toJSON LowerRoman = toJSON ("LowerRoman" :: Text)

instance ToJSON DelimiterType where
    toJSON Period = toJSON ("Period" :: Text)
    toJSON OneParen = toJSON ("OneParen" :: Text)
    toJSON TwoParens = toJSON ("TwoParens" :: Text)

instance ToJSON ListSpacing where
    toJSON TightList = toJSON ("TightList" :: Text)
    toJSON LooseList = toJSON ("LooseList" :: Text)

instance ToJSON ListType where
    toJSON (BulletList c) = Aeson.object ["tag" .= ("BulletList" :: Text), "contents" .= c]
    toJSON (OrderedList n et dt) =
        Aeson.object
            [ "tag" .= ("OrderedList" :: Text)
            , "contents"
                .= Aeson.toJSON1
                    [ toJSON n
                    , toJSON et
                    , toJSON dt
                    ]
            ]

blockUniplate :: Block a -> [Block a]
blockUniplate blk = mconcat [blockChunkBlocks c | c <- blockChunks blk]

blockChunkBlocks :: BlockChunk a -> [Block a]
blockChunkBlocks = go
  where
    only a = [a]
    nil = []
    go bc = case bc of
        BlockQuote bl -> only bl
        List _ _ bls -> bls
        _ -> nil

blockUniverse :: Block a -> [Block a]
blockUniverse blk = blk : mconcat [blockUniverse b | b <- blockUniplate blk]

blockInlines :: Block a -> [Inline a]
blockInlines root = do
    -- list monad!
    blk <- blockUniverse root
    chunk <- blockChunks blk
    il <- blockChunkInlines chunk
    pure il

block :: a -> BlockChunk a -> Block a
block a c = Block [c] mempty a

block0 :: BlockChunk () -> Block ()
block0 = block ()

instance Semigroup (Block ()) where
    Block c1s a1s _ <> Block c2s a2s _ = Block (c1s <> c2s) (a1s <> a2s) ()

instance Monoid (Block ()) where
    mempty = Block mempty mempty ()

instance HasAttributes (Block ()) where
    addAttributes xs (Block c ys _) = Block c (xs <> ys) ()

instance Rangeable (Block ()) where
    ranged _ a = a

instance IsBlock (Inline ()) (Block ()) where
    paragraph = block0 . Paragraph
    plain = block0 . Plain
    thematicBreak = block0 ThematicBreak
    blockQuote = block0 . BlockQuote
    codeBlock t1 t2 = block0 $ CodeBlock t1 t2
    heading n = block0 . Heading n
    rawBlock fmt = block0 . RawBlock fmt
    referenceLinkDefinition lbl dsttile = block0 $ ReferenceLinkDefinition lbl dsttile
    list ltype lspacing = block0 . List ltype lspacing
