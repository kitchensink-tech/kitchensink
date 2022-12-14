module KitchenSink.Core.Assembler.Sections.Json where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText

import KitchenSink.Core.Build.Target
import KitchenSink.Core.Build.Site
import KitchenSink.Core.Section
import KitchenSink.Prelude
import KitchenSink.Core.Assembler.Sections.Primitives

jsonSection :: FromJSON a => Section ext [Text] -> Assembler ext (Section ext a)
jsonSection (Section ty Json lines) =
   let res = Aeson.eitherDecode' (LText.encodeUtf8 $ LText.fromStrict $ Text.unlines lines) in
   case res of
     Left err -> Assembler $ Left (JsonDecodingError err)
     Right a -> Assembler $ Right $ Section ty InMemory a
jsonSection (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

json :: (Eq ext, FromJSON a) => Article ext [Text] -> SectionType ext -> Assembler ext (Section ext a)
json art k = getSection art k >>= jsonSection

jsonm :: (Eq ext, FromJSON a) => Article ext [Text] -> SectionType ext -> Assembler ext (Maybe (Section ext a))
jsonm art k = lookupSection art k >>= traverse jsonSection
