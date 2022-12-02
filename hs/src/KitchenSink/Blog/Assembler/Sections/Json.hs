module KitchenSink.Blog.Assembler.Sections.Json where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText

import KitchenSink.Blog.Build.Target
import KitchenSink.Blog.Build.Site
import KitchenSink.Blog.Section
import KitchenSink.Blog.Prelude
import KitchenSink.Blog.Assembler.Sections.Primitives

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
