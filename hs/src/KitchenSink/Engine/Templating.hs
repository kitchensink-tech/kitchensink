{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Section evaluation backed by @templating-hs@ (the Haskell half of
<https://github.com/lucasdicioccio/templating-lang>), the alternative to the
Dhall backend in "KitchenSink.Engine.SiteLoader".

Two modes, one per 'KitchenSink.Core.Section.Format' constructor:

  * 'evalJsonSection' ('Templating') parses an /expression/-rooted program and
    evaluates it to a JSON value, the same @{format, contents}@ contract Dhall
    sections answer with.
  * 'evalDocSection' ('TemplatingDoc') parses an /element/-rooted program and
    folds the resulting document tree to HTML.

Both receive the same context, 'buildContext', which is the @kitchensink@
record Dhall sections get, reachable as @$ctx.datasets@, @$ctx.vars@,
@$ctx.file@ and @$ctx.sectionNum@.
-}
module KitchenSink.Engine.Templating (
    TemplatingError (..),
    buildContext,
    evalJsonSection,
    evalDocSection,
    renderNodeHtml,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LByteString
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Lucid.Base qualified as Lucid
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Prelude (Integer, (&&), (||))

import Templating.Ast (ActionPayload (..), Node (..))
import Templating.Eval qualified as Templating
import Templating.Parser qualified as Templating

import KitchenSink.Prelude

data TemplatingError
    = TemplatingParseError String
    | TemplatingEvalError Templating.EvalError
    | -- | attribute names the language allows but HTML does not
      TemplatingInvalidAttributeNames [Text]
    deriving (Show, Exception)

{- | The @$ctx@ a templating section is evaluated against, field-for-field the
@kitchensink@ record the Dhall backend builds. Unlike the Dhall path this needs
no schema inference round trip, so there is no way for the datasets to silently
degrade into an error string.
-}
buildContext :: FilePath -> Integer -> [(Text, Text)] -> Map Name Aeson.Value -> Aeson.Value
buildContext path sectionNum vars datasets =
    Aeson.object
        [ ("file", Aeson.toJSON (Text.pack path))
        , ("sectionNum", Aeson.toJSON sectionNum)
        , ("datasets", Aeson.toJSON datasets)
        , ("vars", Aeson.toJSON (Map.fromList vars))
        ]

-- | Expression-rooted mode: the section body evaluates to a JSON value.
evalJsonSection :: Aeson.Value -> Text -> Either TemplatingError Aeson.Value
evalJsonSection ctx body = do
    prog <- adaptParse (Templating.parseJsonProgram body)
    adaptEval (Templating.evalJsonProgram mempty ctx prog)

-- | Element-rooted mode: the section body evaluates to a document tree, which
-- this renders as HTML.
evalDocSection :: Aeson.Value -> Text -> Either TemplatingError Text
evalDocSection ctx body = do
    prog <- adaptParse (Templating.parseProgram body)
    node <- adaptEval (Templating.evalProgram mempty ctx prog)
    renderNodeHtml node

adaptParse :: Either (ParseErrorBundle Text Void) a -> Either TemplatingError a
adaptParse = either (Left . TemplatingParseError . errorBundlePretty) Right

adaptEval :: Either Templating.EvalError a -> Either TemplatingError a
adaptEval = either (Left . TemplatingEvalError) Right

{- | Folds an evaluated document tree to HTML.

Attribute names are validated first: the language permits keys the DOM rejects
(the PureScript host has a @validateAttrNames@ pass for exactly this), and here
they would silently produce malformed markup instead of an error.
-}
renderNodeHtml :: Node -> Either TemplatingError Text
renderNodeHtml node =
    case badAttributeNames node of
        [] -> Right $ LText.toStrict $ Lucid.renderText $ nodeHtml node
        bad -> Left $ TemplatingInvalidAttributeNames bad

badAttributeNames :: Node -> [Text]
badAttributeNames (NText _) = []
badAttributeNames n =
    [k | k <- Map.keys n.neAttrs, not (isValidAttributeName k)]
        <> foldMap badAttributeNames n.neChildren

isValidAttributeName :: Text -> Bool
isValidAttributeName k =
    not (Text.null k) && Text.all ok k
  where
    ok c = isAlphaNum c || c == '-' || c == '_'

nodeHtml :: Node -> Lucid.Html ()
nodeHtml (NText txt) = Lucid.toHtml txt
nodeHtml n
    | isVoidElement n.neTag = Lucid.with (Lucid.makeElementNoEnd n.neTag) attrs
    | otherwise = Lucid.with (Lucid.makeElement n.neTag) attrs (traverse_ nodeHtml n.neChildren)
  where
    attrs :: [Lucid.Attribute]
    attrs =
        [Lucid.makeAttribute k v | (k, v) <- Map.toList n.neAttrs]
            <> maybe [] actionAttributes n.neAction

{- | A statically-produced page has no dispatcher to bind @action(...)@ to, so
rather than dropping the evaluated payload we hand it to the page as data
attributes: that is the seam a JS widget can pick up client-side.
-}
actionAttributes :: ActionPayload -> [Lucid.Attribute]
actionAttributes ap =
    [ Lucid.makeAttribute "data-ks-action-event" ap.apEventType
    , Lucid.makeAttribute "data-ks-action-key" ap.apKey
    , Lucid.makeAttribute "data-ks-action-payload" (encodeJsonText ap.apPayload)
    ]

encodeJsonText :: Aeson.Value -> Text
encodeJsonText = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode

isVoidElement :: Text -> Bool
isVoidElement tag = tag `elem` voidElements

voidElements :: [Text]
voidElements =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "source"
    , "track"
    , "wbr"
    ]
