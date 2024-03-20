{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module KitchenSink.Engine.Api where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prod.Status qualified as Prod
import Servant

import KitchenSink.Engine.Config (Command)
import KitchenSink.Engine.Track (WatchResult)
import KitchenSink.Prelude
import Prod.Proxy qualified as ProdProxy

type DevApi =
    DevWatchApi
        :<|> DevListTargetsApi
        :<|> DevProduceApi
        :<|> DevPublishApi
        :<|> DevListCommandsApi
        :<|> DevExecCommandApi
        :<|> DevForceReloadApi
        :<|> ProxyApi
        :<|> OnTheFlyProductionApi

type ServeApi =
    ProxyApi
        :<|> OnTheFlyProductionApi

type TargetPathName = Text

type DevWatchApi = "dev" :> "watch" :> QueryParam "server-id" Text :> QueryParam "pathname" TargetPathName :> Get '[JSON] (Prod.Identification, WatchResult)

type DevListTargetsApi = "dev" :> "targets" :> Get '[JSON] [Text]

newtype DevTextOutput = DevTextOutput Text
    deriving (Generic, Show)
instance ToJSON DevTextOutput

type DevProduceApi = "dev" :> "produce" :> Post '[JSON] DevTextOutput

type DevPublishApi = "dev" :> "publish" :> Post '[JSON] DevTextOutput

type DevListCommandsApi = "dev" :> "commands" :> Get '[JSON] [Command]

type DevExecCommandApi = "dev" :> "command" :> QueryParam' '[Required, Strict] "handle" Text :> Post '[JSON] DevTextOutput

data ForceReloadStatus = ForceReloaded
    deriving (Generic, Show)
instance ToJSON ForceReloadStatus

type DevForceReloadApi = "dev" :> "reload" :> Post '[JSON] (Maybe ForceReloadStatus)

type ProxyApi = "api" :> ProdProxy.Api

type OnTheFlyProductionApi = Raw
