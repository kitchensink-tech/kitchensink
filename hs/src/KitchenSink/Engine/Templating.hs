{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Section evaluation backed by @tramaj-hs@ (the Haskell half of
<https://github.com/lucasdicioccio/tramaj>), the alternative to the
Dhall backend in "KitchenSink.Engine.SiteLoader".

Two modes, one per 'KitchenSink.Core.Section.Format' constructor:

  * 'evalJsonSection' ('Templating') expects the program to evaluate to a
    plain JSON value, the same @{format, contents}@ contract Dhall sections
    answer with.
  * 'evalDocSection' ('TemplatingDoc') expects the program to evaluate to a
    document tree, and folds it to HTML.

tramaj has a single grammar and evaluator: which of the two a program
produces follows from the value its root actually evaluated to (a 'Node' or
a plain JSON value), not from which of these two entry points was called.
Both below enforce the shape their caller expects, converting the other one
over rather than rejecting it, since both are meaningful JSON/HTML values in
their own right.

Both receive the same context, 'buildContext', which is the @kitchensink@
record Dhall sections get, reachable as @$ctx.datasets@, @$ctx.vars@,
@$ctx.file@ and @$ctx.sectionNum@.
-}
module KitchenSink.Engine.Templating (
    TemplatingError (..),
    buildContext,
    evalJsonSection,
    evalDocSection,
    parseLibrarySection,
    renderNodeHtml,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LByteString
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Lucid.Base qualified as Lucid
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Prelude (Double, Integer, (&&), (||))

import Tramaj.Ast (Program)
import Tramaj.Eval qualified as Templating
import Tramaj.Node (Node (..), NodeAttribute (..))
import Tramaj.Node qualified as Templating
import Tramaj.Parser qualified as Templating

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

-- | Expression-rooted mode: the section body evaluates to a JSON value. A
-- program whose root instead evaluated to a document is accepted too, folded
-- to its normative JSON representation ("Tramaj.Node"'s @nodeToJson@).
evalJsonSection :: Templating.LibraryTable -> Aeson.Value -> Text -> Either TemplatingError (Program, Aeson.Value)
evalJsonSection libtable ctx body = do
    prog <- adaptParse (Templating.parseProgram body)
    out <- adaptEval (Templating.evalProgram Templating.Concrete libtable ctx prog)
    let j = case out of
            Templating.OValue v -> v
            Templating.ONode n -> Templating.nodeToJson n
    pure (prog, j)

-- | Element-rooted mode: the section body evaluates to a document tree, which
-- this renders as HTML. A program whose root evaluated to a plain value
-- instead of a document is accepted too, wrapped as a lone text node.
evalDocSection :: Templating.LibraryTable -> Aeson.Value -> Text -> Either TemplatingError (Program, Text)
evalDocSection libtable ctx body = do
    prog <- parseLibrarySection body
    out <- adaptEval (Templating.evalProgram Templating.Concrete libtable ctx prog)
    let node = case out of
            Templating.ONode n -> n
            Templating.OValue v -> NText v Templating.noAnnotations
    txt <- renderNodeHtml node
    pure (prog, txt)

parseLibrarySection :: Text -> Either TemplatingError Program
parseLibrarySection body =
    adaptParse (Templating.parseProgram body)

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
badAttributeNames (NText _ _) = []
badAttributeNames (NFragment children _) = foldMap badAttributeNames children
badAttributeNames (NElement _ attrs _ children _) =
    [k | NAttr k _ <- attrs, not (isValidAttributeName k)]
        <> foldMap badAttributeNames children

isValidAttributeName :: Text -> Bool
isValidAttributeName k =
    not (Text.null k) && Text.all ok k
  where
    ok c = isAlphaNum c || c == '-' || c == '_'

nodeHtml :: Node -> Lucid.Html ()
nodeHtml (NText v _) = Lucid.toHtml (displayValue v)
nodeHtml (NFragment children _) = traverse_ nodeHtml children
nodeHtml (NElement tag attrs _ children _)
    | isVoidElement tag = Lucid.with (Lucid.makeElementNoEnd tag) htmlAttrs
    | otherwise = Lucid.with (Lucid.makeElement tag) htmlAttrs (traverse_ nodeHtml children)
  where
    htmlAttrs :: [Lucid.Attribute]
    htmlAttrs = foldMap attributeHtml attrs

attributeHtml :: NodeAttribute -> [Lucid.Attribute]
attributeHtml (NAttr k v) = [Lucid.makeAttribute k (displayValue v)]
attributeHtml (NAction event key payload) =
    [ Lucid.makeAttribute "data-ks-action-event" event
    , Lucid.makeAttribute "data-ks-action-key" key
    , Lucid.makeAttribute "data-ks-action-payload" (encodeJsonText payload)
    ]

encodeJsonText :: Aeson.Value -> Text
encodeJsonText = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode

{- | A best-effort rendering of a JSON scalar\/structure to display text, used
for an 'NText' value and an ordinary attribute's value. Mirrors tramaj's own
@str@ builtin closely enough for a demonstrator (a plain string passes
through unchanged, @null@\/booleans render as their literal spelling), but
does not attempt to reproduce its ECMAScript-exact number formatting since
that logic is internal to "Tramaj.Eval" and not exported.
-}
displayValue :: Aeson.Value -> Text
displayValue Aeson.Null = ""
displayValue (Aeson.Bool b) = if b then "true" else "false"
displayValue (Aeson.String s) = s
displayValue (Aeson.Number n) =
    case Scientific.floatingOrInteger n of
        Right (i :: Integer) -> Text.pack (show i)
        Left (_ :: Double) -> Text.pack (show n)
displayValue v = Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode v

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
