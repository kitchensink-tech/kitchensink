{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module KitchenSink.Engine.OnTheFly where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Text (Text)
import Network.HTTP.Types (status200, status404)
import Network.Wai as Wai
import Prod.Tracer
import qualified Prometheus as Prometheus

import KitchenSink.Prelude
import KitchenSink.Core.Build.Target (Target, destinationUrl, destination)
import KitchenSink.Engine.SiteBuilder
import KitchenSink.Engine.Counters (Counters(..), timeItWithLabel)
import KitchenSink.Engine.SiteLoader (Site)
import KitchenSink.Engine.Track (DevServerTrack(..), RequestedPath(..), rootRequestPath, requestedPath, blogTargetTracer)
import KitchenSink.Engine.Runtime

type TargetPath = ByteString

type FetchTarget ext = RequestedPath -> IO (TargetPath, Maybe (Target ext ()))

findTarget
  :: Engine ext
  -> IO (Site ext)
  -> Prod.Tracer.Tracer IO (DevServerTrack ext)
  -> FetchTarget ext
findTarget engine loadSite track = \origpath -> do
  runTracer track (TargetRequested origpath)
  let path = if origpath == rootRequestPath then "/index.html" else coerce origpath
  tgts <- evalTargets engine <$> execLoadMetaExtradata engine <*> loadSite
  let target = List.find (\tgt -> Text.encodeUtf8 (destinationUrl (destination tgt)) == path) tgts
  pure (path, target)

data OnTheFlyCounters a
  = OnTheFlyCounters
  { incTargetRequests :: (Text,Text) -> IO ()
  , timeBuild         :: (Text) -> IO a -> IO a
  }

ontheflyCounters :: Counters -> OnTheFlyCounters a
ontheflyCounters cntrs =
  OnTheFlyCounters
    (\labels -> Prometheus.withLabel cntrs.cnt_targetRequests labels Prometheus.incCounter)
    (\labels work -> timeItWithLabel cntrs.time_ontheflybuild labels work)

handleOnTheFlyProduction
  :: forall ext. (Show ext, Typeable ext)
  => FetchTarget ext
  -> OnTheFlyCounters (LByteString.ByteString, Int64)
  -> Prod.Tracer.Tracer IO (DevServerTrack ext)
  -> Application
handleOnTheFlyProduction fetchTarget cntrs2 track = go
  where
    go :: Application
    go req resp = do
      let origpath = requestedPath req
      (path, target) <- fetchTarget origpath
      maybe
        (handleNotFound origpath resp)
        (handleFound path resp)
        target

    handleNotFound :: RequestedPath -> (Wai.Response -> IO a) -> IO a
    handleNotFound path resp = do
      cntrs2.incTargetRequests ("not-found", Text.decodeUtf8 $ coerce path)
      runTracer track (TargetMissing $ coerce path)
      resp $ Wai.responseLBS status404 [] "not found"

    handleFound :: TargetPath -> (Wai.Response -> IO a) -> Target ext () -> IO a
    handleFound path resp tgt = do
      cntrs2.incTargetRequests ("found", Text.decodeUtf8 path)
      let produce work = cntrs2.timeBuild (destinationUrl $ destination tgt) work
      (body,size) <- produce $ do
        body <- LByteString.fromStrict <$> outputTarget (blogTargetTracer track) tgt
        let size = LByteString.length body
        seq size (pure (body,size))
      runTracer track (TargetBuilt path size)
      resp $ Wai.responseLBS status200 [("content-type", ctypeFor path)] body

    ctypeFor path
      | ".js" `ByteString.isSuffixOf` path = "application/javascript"
      | ".json" `ByteString.isSuffixOf` path = "application/json"
      | ".html" `ByteString.isSuffixOf` path = "text/html"
      | True = ""


