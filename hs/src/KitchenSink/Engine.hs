{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KitchenSink.Engine where

import Options.Generic

import Data.Text as Text
import KitchenSink.Engine.MultiSite qualified as MultiSite
import KitchenSink.Engine.Produce qualified as Produce
import KitchenSink.Engine.Serve qualified as Serve
import KitchenSink.Prelude

data Action
    = Produce
        { srcDir :: FilePath <?> "source directory"
        , outDir :: FilePath <?> "output directory"
        , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
        , var :: [Text] <?> "variables in --var varname=value format"
        }
    | Serve
        { srcDir :: FilePath <?> "source directory"
        , outputDir :: Maybe FilePath <?> "output directory"
        , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
        , servMode :: Serve.ServMode <?> "SERVE|DEV"
        , var :: [Text] <?> "variables in --var varname=value format"
        , httpPort :: Maybe Int <?> "port-num"
        , httpsPort :: Maybe Int <?> "port-num"
        , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
        , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
        }
    | MultiSite
        { configFile :: FilePath <?> "dhall config file"
        , var :: [Text] <?> "variables in --var varname=value format"
        , httpPort :: Maybe Int <?> "port-num"
        , httpsPort :: Maybe Int <?> "port-num"
        , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
        , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
        , proxyingTimeout :: Maybe Int <?> "proxy-timeout-microsecs"
        }
    deriving (Generic, Show)

instance ParseRecord Action

defaultMain :: IO ()
defaultMain = do
    cmd <- getRecord "kitchen-sink"
    case cmd of
        Produce a b c vars ->
            let (a', b', c') = coerce (a, b, c)
             in Produce.run (Produce.Args a' b' c' (parseVars (coerce vars)))
        Serve a b c d vars e f g h ->
            let (a', b', c', d', e', f', g', h') = coerce (a, b, c, d, e, f, g, h)
             in Serve.run (Serve.Args a' b' c' d' (parseVars (coerce vars)) e' f' g' h')
        MultiSite a vars b c d e f ->
            let (a', b', c', d', e', f') = coerce (a, b, c, d, e, f)
             in MultiSite.run (MultiSite.Args a' (parseVars (coerce vars)) b' c' d' e' f')
  where
    parseVars :: [Text] -> [(Text, Text)]
    parseVars xs = fmap parseVar xs

    parseVar :: Text -> (Text, Text)
    parseVar txt =
        let (k, v) = Text.breakOn "=" txt
         in (k, Text.drop 1 v)
