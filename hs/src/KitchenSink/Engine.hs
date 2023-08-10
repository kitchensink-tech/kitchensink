{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module KitchenSink.Engine where

import Options.Generic

import KitchenSink.Prelude
import qualified KitchenSink.Engine.MultiSite as MultiSite
import qualified KitchenSink.Engine.Produce as Produce
import qualified KitchenSink.Engine.Serve as Serve

data Action
  = Produce
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
    }
  | Serve
    { srcDir :: FilePath <?> "source directory" 
    , outDir :: FilePath <?> "output directory" 
    , ksFile :: Maybe FilePath <?> "kitchen-sink.json file"
    , servMode :: Serve.ServMode <?> "SERVE|DEV" 
    , httpPort   :: Maybe Int <?> "port-num"
    , httpsPort   :: Maybe Int <?> "port-num"
    , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
    , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
    }
  | MultiSite
    { configFile :: FilePath <?> "dhall config file"
    , httpPort   :: Maybe Int <?> "port-num"
    , httpsPort   :: Maybe Int <?> "port-num"
    , tlsKeyFile :: Maybe FilePath <?> "tls-private-key"
    , tlsCertFile :: Maybe FilePath <?> "tls-certificate"
    }
  deriving (Generic, Show)

instance ParseRecord Action

defaultMain :: IO ()
defaultMain = do
  cmd <- getRecord "kitchen-sink"
  case cmd of
    Produce a b c -> Produce.run (Produce.Args a b c)
    Serve a b c d e f g h -> Serve.run (Serve.Args a b c d e f g h)
    MultiSite a b c d e -> MultiSite.run (MultiSite.Args a b c d e)
