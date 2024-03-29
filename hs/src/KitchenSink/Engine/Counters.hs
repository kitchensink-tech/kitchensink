{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Counters (Counters (..), initCounters, timeItWithLabel) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Real (fromRational, toRational)
import Prometheus qualified as Prometheus

import KitchenSink.Prelude

data Counters
    = Counters
    { time_singlebuild :: Prometheus.Vector Text Prometheus.Summary
    , time_fullbuild :: Prometheus.Summary
    , time_publishing :: Prometheus.Summary
    , time_ontheflybuild :: Prometheus.Vector Text Prometheus.Summary
    , cnt_reloads :: Prometheus.Counter
    , cnt_watches :: Prometheus.Vector Text Prometheus.Counter
    , cnt_rebuilds :: Prometheus.Counter
    , cnt_publishs :: Prometheus.Counter
    , cnt_targetRequests :: Prometheus.Vector (Text, Text) Prometheus.Counter
    , cnt_targetSizes :: Prometheus.Vector (Text) Prometheus.Gauge
    , cnt_sources :: Prometheus.Gauge
    , cnt_forceReloads :: Prometheus.Counter
    , cnt_commands :: Prometheus.Vector Text Prometheus.Counter
    }
initCounters :: IO Counters
initCounters =
    Counters
        <$> reg1s "ks_single_build_time" "path" "time spent building single targets on disk"
        <*> reg0s "ks_fullbuild_time" "time spent building targets on disk"
        <*> reg0s "ks_publish_time" "time spent publishing"
        <*> reg1s "ks_ontheflybuild_time" "path" "time spent building targets on disk"
        <*> reg0 "ks_reloads" "number of time the site is reloaded"
        <*> reg1 "ks_watches" "status" "number of watches created"
        <*> reg0 "ks_rebuilds" "number of time the output has been rebuilt"
        <*> reg0 "ks_publishes" "number of time the site has been published"
        <*> reg1 "ks_targets_requests" ("status", "path") "number of queries per blog target"
        <*> reg1g "ks_targets_sizes" ("path") "sizes of targets in bytes"
        <*> reg0g "ks_targets_number" "number of targets"
        <*> reg0 "ks_forceReloads" "number of time the site has been reloaded upon user request"
        <*> reg1 "ks_commands" "status" "number of command ran"
  where
    reg0 k h =
        Prometheus.register
            $ Prometheus.counter (Prometheus.Info k h)
    reg0g k h =
        Prometheus.register
            $ Prometheus.gauge (Prometheus.Info k h)
    reg1 k t h =
        Prometheus.register
            $ Prometheus.vector t
            $ Prometheus.counter (Prometheus.Info k h)
    reg1g k t h =
        Prometheus.register
            $ Prometheus.vector t
            $ Prometheus.gauge (Prometheus.Info k h)
    reg0s k h =
        Prometheus.register
            $ Prometheus.summary (Prometheus.Info k h) Prometheus.defaultQuantiles
    reg1s k t h =
        Prometheus.register
            $ Prometheus.vector t
            $ Prometheus.summary (Prometheus.Info k h) Prometheus.defaultQuantiles

timeItWithLabel ::
    (Prometheus.Label label, Prometheus.Observer metric) =>
    Prometheus.Vector label metric ->
    label ->
    IO b ->
    IO b
timeItWithLabel cnt lbl action = do
    t0 <- liftIO $ getCurrentTime
    !ret <- action
    t1 <- liftIO $ getCurrentTime
    Prometheus.withLabel cnt lbl $ flip Prometheus.observe (fromRational $ toRational $ diffUTCTime t1 t0)
    pure ret
