{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KitchenSink.Engine.Init where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List qualified as List
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import Prelude ((&&))

import KitchenSink.Prelude

data Args
    = Args
    { dir :: FilePath
    , force :: Bool
    }

-- | Every file under hs/scaffolding/ (a copy of the repo-root scaffolding/
-- kept in sync by scripts/import-scaffold.sh), embedded at compile-time so
-- `init` works regardless of the current working directory or install
-- location. It must live inside the package directory: `cabal install`
-- builds from a temp copy of just the package, so a path reaching outside
-- it (e.g. "../scaffolding") is not available at TH-splice time.
scaffoldingFiles :: [(FilePath, ByteString)]
scaffoldingFiles = $(makeRelativeToProject "scaffolding" >>= embedDir)

-- | Directory skeleton that produce/serve expect to already exist
-- (nothing in the engine creates these on demand).
outputSkeletonDirs :: [FilePath]
outputSkeletonDirs =
    [ ""
    , "gen"
    , "gen/out"
    , "gen/images"
    , "json"
    , "js"
    , "css"
    , "webfonts"
    , "raw"
    , "raw/data"
    , ".well-known"
    , "topics"
    , "hashtags"
    , "images"
    , "videos"
    , "audios"
    , "docs"
    , "text"
    ]

run :: Args -> IO ()
run cmd = do
    let target = cmd.dir
    let srcDir = target </> "src"
    let outDir = target </> "www"
    alreadyExists <- doesDirectoryExist target
    nonEmpty <-
        if alreadyExists
            then not . List.null <$> listDirectory target
            else pure False
    if nonEmpty && not cmd.force
        then do
            hPutStrLn stderr $
                target
                    <> " already exists and is not empty; pass --force to overwrite"
            exitFailure
        else do
            createDirectoryIfMissing True srcDir
            traverse_ (createDirectoryIfMissing True . (outDir </>)) outputSkeletonDirs
            traverse_ (writeScaffoldFile target srcDir) scaffoldingFiles
            putStrLn $ "Created new kitchen-sink site in " <> target
            putStrLn ""
            putStrLn "Next steps:"
            putStrLn $ "  cd " <> target
            putStrLn $
                "  kitchen-sink serve --srcDir src --outputDir www --servMode DEV --httpPort 7655"

-- | Reproduces the mapping of scaffolding/sourcedir.sh: .tmpl files are
-- renamed on copy, and js/ and css/ subdirectories are flattened into the
-- source directory root. README.md is the one exception, landing at the
-- top of the bootstrapped project rather than inside src/.
writeScaffoldFile :: FilePath -> FilePath -> (FilePath, ByteString) -> IO ()
writeScaffoldFile target srcDir (relPath, contents)
    | relPath == "README.md" = ByteString.writeFile (target </> "README.md") contents
    | otherwise = ByteString.writeFile (srcDir </> destinationFileName relPath) contents

destinationFileName :: FilePath -> FilePath
destinationFileName relPath
    | relPath == "index.cmark.tmpl" = "index.cmark"
    | relPath == "glossary.cmark.tmpl" = "glossary.cmark"
    | relPath == "topics.cmark.tmpl" = "topics.cmark"
    | relPath == "hashtags.cmark.tmpl" = "hashtags.cmark"
    | relPath == "newpage.cmark.tmpl" = "first-article.cmark"
    | otherwise = takeFileName relPath
