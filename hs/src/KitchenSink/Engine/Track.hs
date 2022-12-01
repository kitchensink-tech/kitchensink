
module KitchenSink.Engine.Track where

import Data.Aeson (ToJSON)

import KitchenSink.Blog.Prelude
import KitchenSink.Blog.SiteLoader as SiteLoader
import KitchenSink.Blog.Target hiding (Tracer)
import KitchenSink.Engine.Config (Command)

import Prod.Background as Background
import Data.ByteString (ByteString)

import Data.Int (Int64)

import qualified System.FSNotify as FSNotify


import GHC.Generics (Generic)
import Data.Text (Text)

data DevServerTrack
  = ProducedBuild
  | PublishedBuild String
  | WatchAdded Text (Maybe Text)
  | WatchRejected (Maybe Text) (Maybe Text)
  | WatchLeft (Maybe Text) WatchResult
  | FileWatch FSNotify.Event
  | SiteReloaded (Background.Track ())
  | TargetRequested ByteString
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
