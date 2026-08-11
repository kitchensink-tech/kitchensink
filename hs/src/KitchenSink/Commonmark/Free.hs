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

{- Note: neither 'Inline' nor 'Block' may be parameterized.

commonmark >= 0.3 ships an OVERLAPPABLE @IsBlock (f il) (f b)@ instance lifting
the class through applicative functors. 'IsBlock' has the functional dependency
@b -> il@, so a parameterized @Block a@ would unify with @f b@ and be forced to
pick @il ~ Block _@, conflicting with the @Inline@ instance below. GHC's fundep
consistency check runs regardless of OVERLAPPING pragmas, so keeping these type
constructors nullary is what makes the instances legal.
-}

-- INLINES

data InlineChunk
    = LineBreak
    | SoftBreak
    | Str Text
    | Entity Text
    | EscapedChar Char
    | Emph Inline
    | Strong Inline
    | Link Text Text Inline
    | Image Text Text Inline
    | Code Text
    | HashTag Text
    | RawInline Format Text
    | SpanInline Inline
    deriving (Show, Generic)
instance ToJSON InlineChunk

data Inline = Inline
    { inlineChunks :: [InlineChunk]
    , inlineAttributes :: Attributes
    }
    deriving (Show, Generic)
instance ToJSON Inline

instance ToJSON Format where
    toJSON (Format txt) = toJSON txt

inlineUniplate :: Inline -> [Inline]
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

inlineUniverse :: Inline -> [Inline]
inlineUniverse blk = blk : mconcat [inlineUniverse b | b <- inlineUniplate blk]

inline :: InlineChunk -> Inline
inline c = Inline [c] mempty

instance Semigroup Inline where
    Inline c1s a1s <> Inline c2s a2s = Inline (c1s <> c2s) (a1s <> a2s)

instance Monoid Inline where
    mempty = Inline mempty mempty

instance HasAttributes Inline where
    addAttributes xs (Inline c ys) = Inline c (xs <> ys)

instance HasHashTag Inline where
    hashtag t = Inline [HashTag t] mempty

instance HasSpan Inline where
    spanWith attrs il = Inline [SpanInline il] attrs

instance Rangeable Inline where
    ranged _ a = a

instance IsInline Inline where
    lineBreak = inline LineBreak
    softBreak = inline SoftBreak
    str = inline . Str
    entity = inline . Entity
    escapedChar = inline . EscapedChar
    emph = inline . Emph
    strong = inline . Strong
    link dst title = inline . Link dst title
    image dst title = inline . Image dst title
    code = inline . Code
    rawInline fmt = inline . RawInline fmt

-- BLOCKS

data BlockChunk
    = Paragraph Inline
    | Plain Inline
    | ThematicBreak
    | BlockQuote Block
    | CodeBlock Text Text
    | Heading Int Inline
    | RawBlock Format Text
    | ReferenceLinkDefinition Text (Text, Text)
    | List ListType ListSpacing [Block]
    | NestedDivBlock Block
    deriving (Show, Generic)
instance ToJSON BlockChunk

blockChunkInlines :: BlockChunk -> [Inline]
blockChunkInlines bc =
    let only a = [a]
        nil = []
     in case bc of
            Paragraph il -> only il
            Plain il -> only il
            Heading _ il -> only il
            _ -> nil

data Block = Block
    { blockChunks :: [BlockChunk]
    , blockAttributes :: Attributes
    }
    deriving (Show, Generic)
instance ToJSON Block

instance HasDiv Block where
    div_ bl = Block [NestedDivBlock bl] mempty

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

blockUniplate :: Block -> [Block]
blockUniplate blk = mconcat [blockChunkBlocks c | c <- blockChunks blk]

blockChunkBlocks :: BlockChunk -> [Block]
blockChunkBlocks = go
  where
    only a = [a]
    nil = []
    go bc = case bc of
        BlockQuote bl -> only bl
        List _ _ bls -> bls
        _ -> nil

blockUniverse :: Block -> [Block]
blockUniverse blk = blk : mconcat [blockUniverse b | b <- blockUniplate blk]

blockInlines :: Block -> [Inline]
blockInlines root = do
    -- list monad!
    blk <- blockUniverse root
    chunk <- blockChunks blk
    il <- blockChunkInlines chunk
    pure il

block :: BlockChunk -> Block
block c = Block [c] mempty

instance Semigroup Block where
    Block c1s a1s <> Block c2s a2s = Block (c1s <> c2s) (a1s <> a2s)

instance Monoid Block where
    mempty = Block mempty mempty

instance HasAttributes Block where
    addAttributes xs (Block c ys) = Block c (xs <> ys)

instance Rangeable Block where
    ranged _ a = a

instance IsBlock Inline Block where
    paragraph = block . Paragraph
    plain = block . Plain
    thematicBreak = block ThematicBreak
    blockQuote = block . BlockQuote
    codeBlock t1 t2 = block $ CodeBlock t1 t2
    heading n = block . Heading n
    rawBlock fmt = block . RawBlock fmt
    referenceLinkDefinition lbl dsttile = block $ ReferenceLinkDefinition lbl dsttile
    list ltype lspacing = block . List ltype lspacing
