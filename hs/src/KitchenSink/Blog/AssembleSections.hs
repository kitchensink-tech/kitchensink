module KitchenSink.Blog.AssembleSections where

import qualified Commonmark
import qualified Commonmark.Extensions as CommonmarkE
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.List as List

import KitchenSink.Blog.Target
import KitchenSink.Blog.Section
import KitchenSink.Blog.Site
import KitchenSink.Blog.Prelude
import qualified KitchenSink.Commonmark.Free as FreeCommonmark

import KitchenSink.Commonmark.BlogHTML


jsonSection :: FromJSON a => Section [Text] -> Assembler (Section a)
jsonSection (Section ty Json lines) =
   let res = Aeson.eitherDecode' (LText.encodeUtf8 $ LText.fromStrict $ Text.unlines lines) in
   case res of
     Left err -> Assembler $ Left (JsonDecodingError err)
     Right a -> Assembler $ Right $ Section ty InMemory a
jsonSection (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

json :: FromJSON a => Article [Text] -> SectionType -> Assembler (Section a)
json art k = getSection art k >>= jsonSection

jsonm :: FromJSON a => Article [Text] -> SectionType -> Assembler (Maybe (Section a))
jsonm art k = lookupSection art k >>= traverse jsonSection

newtype PreRenderedHtml = PreRenderedHtml Text

dumpCMark :: Section [Text] -> Assembler (Section (FreeCommonmark.Block ()))
dumpCMark (Section ty Cmark lines) = do
   let customSyntax = Commonmark.defaultSyntaxSpec
   res <- Commonmark.commonmarkWith customSyntax "inline" (Text.unlines lines)
   case res of
     Left err -> Assembler $ Left (CommonMarkRenderingError err)
     Right (blk :: FreeCommonmark.Block ()) -> Assembler $ Right $ Section ty InMemory  blk
dumpCMark (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

parseCMark :: Section [Text] -> Assembler (Section Html)
parseCMark (Section ty Cmark lines) = do
   let customSyntax =
           mconcat
           [ CommonmarkE.fencedDivSpec
           , CommonmarkE.bracketedSpanSpec
           , CommonmarkE.emojiSpec
           , CommonmarkE.smartPunctuationSpec
           , CommonmarkE.autoIdentifiersSpec
           , CommonmarkE.implicitHeadingReferencesSpec
           , Commonmark.defaultSyntaxSpec
           ]
   res <- Commonmark.commonmarkWith customSyntax "inline" (Text.unlines lines)
   case res of
     Left err -> Assembler $ Left (CommonMarkRenderingError err)
     Right (html :: Html) -> Assembler $ Right $ Section ty InMemory html
parseCMark (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

renderSection :: Section [Text] -> Assembler (Section PreRenderedHtml)
renderSection s@(Section _ encoding _) =
  case encoding of
    Cmark -> renderCMark s
    TextHtml -> Assembler $ Right $ fmap (coerce Text.unlines) s
    fmt -> Assembler $ Left (UnsupportedConversionFormat fmt)

renderCMark :: Section [Text] -> Assembler (Section PreRenderedHtml)
renderCMark sec =
    fmap (fmap r) $ parseCMark sec
  where
    r (html :: Html) =
        PreRenderedHtml
        $ LText.toStrict
        $ renderHtml html

getSection :: Article a -> SectionType -> Assembler (Section a)
getSection (Article _ xs) tyA =
    Assembler
    $ maybe (Left $ SectionNotFound tyA) Right
    $ List.find f xs
  where
    f (Section tyB  _ _) = tyB == tyA

getSections :: Article a -> SectionType -> Assembler [Section a]
getSections (Article _ xs) tyA =
    pure
    $ List.filter f xs
  where
    f (Section tyB  _ _) = tyB == tyA

lookupSection :: Article a -> SectionType -> Assembler (Maybe (Section a))
lookupSection (Article _ xs) tyA =
    pure $ List.find f xs
  where
    f (Section tyB  _ _) = tyB == tyA
