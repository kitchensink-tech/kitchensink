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
@$ctx.file@, @$ctx.sectionNum@ and @$ctx.pathPrefix@.

Values crossing into and out of tramaj are "Tramaj.Json" values, not aeson
ones: tramaj has two number types (an integer, and a float written with a
fraction or an exponent, @1@ and @1.0@ being two different values), which an
'Aeson.Value' cannot tell apart. The datasets the context carries are read
with tramaj's own 'Tramaj.Json.jsonParser' for that reason.
-}
module KitchenSink.Engine.Templating (
    TemplatingError (..),
    buildContext,
    evalJsonSection,
    evalDocSection,
    parseLibrarySection,
    renderNodeHtml,
) where

import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Lucid.Base qualified as Lucid
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Prelude (Integer, (&&), (||))

import Tramaj.Ast (Program)
import Tramaj.Eval qualified as Templating
import Tramaj.Json (Json (..))
import Tramaj.Json qualified as Json
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
buildContext :: FilePath -> Integer -> Text -> [(Text, Text)] -> Map Name Json -> Json
buildContext path sectionNum pathPrefix vars datasets =
    JObject
        $ Map.fromList
            [ ("file", JString (Text.pack path))
            , ("sectionNum", JInt sectionNum)
            , ("pathPrefix", JString pathPrefix)
            , ("datasets", JObject datasets)
            , ("vars", JObject (fmap JString (Map.fromList vars)))
            ]

-- | Expression-rooted mode: the section body evaluates to a JSON value. A
-- program whose root instead evaluated to a document is accepted too, folded
-- to its normative JSON representation ("Tramaj.Node"'s @nodeToJson@).
evalJsonSection :: Templating.LibraryTable -> Json -> Text -> Either TemplatingError (Program, Json)
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
evalDocSection :: Templating.LibraryTable -> Json -> Text -> Either TemplatingError (Program, Text)
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
    , Lucid.makeAttribute "data-ks-action-payload" (Json.stringify payload)
    ]

{- | The display text of a value, used for an 'NText' value and an ordinary
attribute's value. It is tramaj's own @str@ rendering ("Tramaj.Json"'s
writer, with a number written by its type: @1@ for an integer, @1.0@ for a
whole-valued float), except that @null@ displays as nothing and a string
passes through unquoted.
-}
displayValue :: Json -> Text
displayValue JNull = ""
displayValue (JString s) = s
displayValue v = Json.stringify v

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
