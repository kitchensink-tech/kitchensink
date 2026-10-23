{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Api where

import Prod.Status qualified as Prod
import Servant

import KitchenSink.Engine.Track (WatchResult)
import KitchenSink.Prelude
import Prod.Proxy qualified as ProdProxy

type DevApi =
    DevWatchApi
        :<|> DevListTargetsApi
        :<|> ProxyApi
        :<|> OnTheFlyProductionApi

type ServeApi =
    ProxyApi
        :<|> OnTheFlyProductionApi

type TargetPathName = Text

type DevWatchApi = "dev" :> "watch" :> QueryParam "server-id" Text :> QueryParam "pathname" TargetPathName :> Get '[JSON] (Prod.Identification, WatchResult)

type DevListTargetsApi = "dev" :> "targets" :> Get '[JSON] [Text]

type ProxyApi = "api" :> ProdProxy.Api

type OnTheFlyProductionApi = Raw
