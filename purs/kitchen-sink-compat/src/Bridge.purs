-- File auto generated by purescript-bridge! --
module Bridge where

import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import KitchenSink.Layout.Blog.Analyses.ArticleInfos (HashTagInfo, ImageInfo, LinkInfo, SnippetInfo)
import KitchenSink.Layout.Blog.Analyses.SkyLine (SkyLine)
import Prim (Array)
import Type.Proxy (Proxy(Proxy))

import Prelude

newtype ArticleInfos =
    ArticleInfos {
      linkInfos :: Array LinkInfo
    , imageInfos :: Array ImageInfo
    , hashtagInfos :: Array HashTagInfo
    , snippetInfos :: Array SnippetInfo
    , skyline :: SkyLine
    }

instance encodeJsonArticleInfos :: EncodeJson ArticleInfos where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions
instance decodeJsonArticleInfos :: DecodeJson ArticleInfos where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions
derive instance genericArticleInfos :: Generic ArticleInfos _
derive instance newtypeArticleInfos :: Newtype ArticleInfos _

--------------------------------------------------------------------------------
_ArticleInfos :: Iso' ArticleInfos { linkInfos :: Array LinkInfo, imageInfos :: Array ImageInfo, hashtagInfos :: Array HashTagInfo, snippetInfos :: Array SnippetInfo, skyline :: SkyLine}
_ArticleInfos = _Newtype

--------------------------------------------------------------------------------
