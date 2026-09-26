{-# LANGUAGE DataKinds #-}

module KitchenSink.Engine.Utils where

import Data.Aeson (FromJSON, decode, eitherDecode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LByteString
import Data.Maybe (fromMaybe)
import KitchenSink.Prelude
import System.FilePath.Posix (takeExtension, (</>))

-- | Loads a JSON configuration file, with a readable reason when it cannot be
-- loaded (including the removed Dhall format).
loadConfigFile :: (FromJSON a) => FilePath -> IO (Either String a)
loadConfigFile path
    | takeExtension path == ".dhall" =
        pure $ Left (path <> ": Dhall configuration files are no longer supported; write the same stanzas as JSON")
    | otherwise = first ((path <> ": ") <>) . eitherDecode <$> LByteString.readFile path

loadJSONFile :: (FromJSON a) => FilePath -> IO (Maybe a)
loadJSONFile path =
    decode <$> LByteString.readFile path

mio :: Maybe (IO ()) -> IO ()
mio = fromMaybe (pure ())

kitshenSinkJsonFilePath ::
    FilePath ->
    Maybe FilePath ->
    FilePath
kitshenSinkJsonFilePath base preferred =
    let fallback = base </> "kitchen-sink.json"
     in fromMaybe fallback (preferred)
