module KitchenSink.Core.Assembler.Sections.Json where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Encoding qualified as LText

import KitchenSink.Core.Assembler.Sections.Primitives
import KitchenSink.Core.Build.Site
import KitchenSink.Core.Build.Target
import KitchenSink.Core.Section
import KitchenSink.Prelude

jsonSection :: (FromJSON a) => Section ext [Text] -> Assembler ext (Section ext a)
jsonSection (Section ty Json lines) =
    let res = Aeson.eitherDecode' (LText.encodeUtf8 $ LText.fromStrict $ Text.unlines lines)
     in case res of
            Left err -> Assembler $ Left (JsonDecodingError err)
            Right a -> Assembler $ Right $ Section ty InMemory a
jsonSection (Section _ fmt _) = Assembler $ Left (UnsupportedConversionFormat fmt)

json :: (Eq ext, FromJSON a) => Article ext [Text] -> SectionPredicate ext -> Assembler ext (Section ext a)
json art k = getSection art k >>= jsonSection

jsonm :: (Eq ext, FromJSON a) => Article ext [Text] -> SectionPredicate ext -> Assembler ext (Maybe (Section ext a))
jsonm art k = lookupSection art k >>= traverse jsonSection
