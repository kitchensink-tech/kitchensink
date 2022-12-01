{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Track where

import Control.Exception (SomeException)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Request, rawPathInfo)
import Prod.Background as Background
import qualified System.FSNotify as FSNotify

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.SiteLoader as SiteLoader
import KitchenSink.Blog.Target hiding (Tracer)
import KitchenSink.Engine.Config (Command)

-- we distinguish requested paths from effective target-path (for counters and other processing)
newtype RequestedPath = RequestedPath ByteString
  deriving (Eq, Ord, Show)

requestedPath :: Request -> RequestedPath
requestedPath = RequestedPath . rawPathInfo

rootRequestPath :: RequestedPath
rootRequestPath = RequestedPath "/"

data DevServerTrack
  = ProducedBuild
  | PublishedBuild String
  | WatchAdded Text (Maybe Text)
  | WatchRejected (Maybe Text) (Maybe Text)
  | WatchLeft (Maybe Text) WatchResult
  | FileWatch FSNotify.Event
  | SiteReloaded (Background.Track ())
  | SiteReloadException SomeException
  | TargetRequested RequestedPath
  | TargetMissing ByteString
  | TargetBuilt ByteString Int64
  | Loading SiteLoader.LogMsg
  | BlogTargetTrace KitchenSink.Blog.Target.Trace
  | CommandRan Command String
  deriving Show

data WatchResult
  = Reloaded
  | Disappeared
  | Respawned
  deriving (Generic, Show)
instance ToJSON WatchResult