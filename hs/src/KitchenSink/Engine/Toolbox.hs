{-# LANGUAGE OverloadedStrings #-}

-- | A single agents-exe bash-toolbox ("describe"/"run") wrapper around
-- kitchen-sink's own one-shot subcommands.
--
-- See <https://github.com/kitchensink-tech/agents-exe> documentation
-- @documentation\/binary-tool.md@ for the protocol this implements: a tool
-- is called with @describe@ (no other args) to print a JSON description of
-- its interface, or with @run@ followed by arguments to execute it, writing
-- its result to stdout.
--
-- This wrapper exposes exactly one argument: the raw kitchen-sink
-- subcommand and flags to run, e.g. @"produce --srcDir website-src --outDir
-- www"@. Only @produce@, @init@ and @newarticle@ are allowed, because
-- @serve@ and @multisite@ start long-running daemons and don't fit a
-- one-shot, stdout-returning toolbox tool.
module KitchenSink.Engine.Toolbox (toolboxMain) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.List (drop, isPrefixOf, words)
import Data.Text qualified as Text
import System.Directory (findExecutable)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (exitFailure, exitWith)
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.IO (hPutStrLn, putStr, stderr)
import System.Process (readProcessWithExitCode)

import KitchenSink.Prelude

-- | Subcommands of the real @kitchen-sink@ binary that are one-shot and
-- batch-friendly, and thus safe to expose through this toolbox tool. These
-- are the exact (lowercase) spellings the @kitchen-sink@ binary itself
-- expects on its command line.
allowedSubcommands :: [Text]
allowedSubcommands = ["produce", "init", "newarticle"]

toolboxMain :: IO ()
toolboxMain = do
    args <- getArgs
    case args of
        ["describe"] -> describe
        ("run" : rest) -> run rest
        _ -> usageError

usageError :: IO ()
usageError = do
    hPutStrLn stderr "usage: kitchen-sink-tool describe | kitchen-sink-tool run --command \"<subcommand and flags>\""
    exitFailure

describe :: IO ()
describe = do
    LBS.putStr (encode descriptionValue)
    putStrLn ""
  where
    descriptionValue =
        object
            [ "slug" .= ("kitchen_sink" :: Text)
            ,
                ( "description"
                    .= ( "Runs a one-shot kitchen-sink subcommand ("
                            <> Text.intercalate ", " allowedSubcommands
                            <> ") and returns its output. kitchen-sink is a static-site"
                            <> " generator; use this to produce a site, scaffold a new one,"
                            <> " or add a new article. Long-running daemons (serve, multisite)"
                            <> " are not supported."
                       )
                )
            ,
                ( "args"
                    .= [ object
                            [ "name" .= ("command" :: Text)
                            ,
                                ( "description"
                                    .= ( "The kitchen-sink subcommand and its flags, exactly as you would"
                                            <> " pass them on the command line, e.g. \"produce --srcDir website-src --outDir www\"."
                                            <> " Only "
                                            <> Text.intercalate ", " allowedSubcommands
                                            <> " are supported."
                                       )
                                )
                            , "type" .= ("string" :: Text)
                            , "backing_type" .= ("string" :: Text)
                            , "arity" .= ("single" :: Text)
                            , "mode" .= ("dashdashspace" :: Text)
                            ]
                       ]
                )
            ]

run :: [String] -> IO ()
run rest = case extractCommand rest of
    Nothing -> do
        hPutStrLn stderr "run: missing --command \"<subcommand and flags>\""
        exitFailure
    Just commandString ->
        case words commandString of
            [] -> do
                hPutStrLn stderr "run: --command was empty"
                exitFailure
            (subcommand : flags)
                | Text.toLower (Text.pack subcommand) `elem` allowedSubcommands -> do
                    kitchenSinkBin <- locateKitchenSink
                    let canonicalSubcommand = Text.unpack (Text.toLower (Text.pack subcommand))
                    (exitCode, out, err) <- readProcessWithExitCode kitchenSinkBin (canonicalSubcommand : flags) ""
                    putStr out
                    hPutStrLn stderr err
                    exitWith exitCode
                | otherwise -> do
                    hPutStrLn stderr $
                        "run: unsupported subcommand "
                            <> subcommand
                            <> " (only "
                            <> Text.unpack (Text.intercalate ", " allowedSubcommands)
                            <> " are supported by this toolbox tool)"
                    exitFailure

-- | Pull the value of @--command@ out of the raw @run@ arguments. Accepts
-- either @--command value@ (two args, the protocol's @dashdashspace@ mode)
-- or @--command=value@ (@dashdashequal@), in case a caller uses that
-- instead.
extractCommand :: [String] -> Maybe String
extractCommand [] = Nothing
extractCommand (x : xs)
    | x == "--command" = case xs of
        (v : _) -> Just v
        [] -> Nothing
    | "--command=" `isPrefixOf` x = Just (drop (length ("--command=" :: String)) x)
    | otherwise = extractCommand xs

-- | Find the real @kitchen-sink@ binary: first next to this executable
-- (the common case, since both are built/installed together), then falling
-- back to @$PATH@.
locateKitchenSink :: IO FilePath
locateKitchenSink = do
    override <- lookupEnv "KITCHEN_SINK"
    case override of
        Just p -> pure p
        Nothing -> do
            self <- getExecutablePath
            let sibling = FilePath.takeDirectory self </> "kitchen-sink"
            fromPath <- findExecutable sibling
            case fromPath of
                Just p -> pure p
                Nothing -> do
                    onPath <- findExecutable "kitchen-sink"
                    case onPath of
                        Just p -> pure p
                        Nothing -> do
                            hPutStrLn stderr "run: could not find the kitchen-sink binary (looked next to this executable and on PATH; set KITCHEN_SINK to override)"
                            exitFailure
