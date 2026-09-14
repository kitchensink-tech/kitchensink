{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.NewArticle where

import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum)
import Data.List (filter)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextLazy
import Data.Text.Lazy.Encoding qualified as TextLazy
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath.Posix ((</>))
import System.IO (hPutStrLn, stderr)

import KitchenSink.Prelude

data Args
    = Args
    { srcDir :: FilePath
    , title :: Text
    , layout :: Maybe Text
    , author :: Maybe Text
    }

run :: Args -> IO ()
run cmd = do
    srcDirExists <- doesDirectoryExist cmd.srcDir
    if not srcDirExists
        then do
            hPutStrLn stderr $ cmd.srcDir <> " does not exist"
            exitFailure
        else do
            let filename = cmd.srcDir </> Text.unpack slug <> ".cmark"
            fileExists <- doesFileExist filename
            if fileExists
                then do
                    hPutStrLn stderr $ filename <> " already exists"
                    exitFailure
                else do
                    now <- getCurrentTime
                    Text.writeFile filename (articleContents layoutName authorName cmd.title now)
                    putStrLn $ "Created new article in " <> filename
  where
    slug = slugify cmd.title
    layoutName = maybe "article" Text.toLower cmd.layout
    authorName = fromMaybe "Anonymous Author" cmd.author

slugify :: Text -> Text
slugify =
    Text.intercalate "-"
        . filter (not . Text.null)
        . Text.split (== '-')
        . Text.map toDash
        . Text.toLower
  where
    toDash c
        | isAlphaNum c = c
        | otherwise = '-'

jsonLine :: [Pair] -> Text
jsonLine = TextLazy.toStrict . TextLazy.decodeUtf8 . encode . object

articleContents :: Text -> Text -> Text -> UTCTime -> Text
articleContents layoutName authorName title now =
    Text.unlines
        [ "=base:build-info.json"
        , jsonLine ["layout" .= layoutName, "publicationStatus" .= ("Public" :: Text)]
        , ""
        , "=base:preamble.json"
        , jsonLine ["author" .= authorName, "title" .= title, "date" .= now]
        , ""
        , "=base:topic.json"
        , jsonLine ["topics" .= ([] :: [Text]), "keywords" .= ([] :: [Text])]
        , ""
        , "=base:summary.cmark"
        , ""
        , "TODO: write a short summary of \"" <> title <> "\"."
        , ""
        , "=base:main-content.cmark"
        , ""
        , "# " <> title
        , ""
        , "TODO: write the article content."
        ]
