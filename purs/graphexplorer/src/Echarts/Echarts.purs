
module Echarts where

import Data.Argonaut.Core (Json)
import Prelude (Unit)
import Effect (Effect)
import Web.DOM.Internal.Types (Element)
import Data.Function.Uncurried (Fn1)

type ChartRef = {}
type EventName = String
type EventQuery = String
type Callback r = Fn1 r (Effect Unit)

foreign import init :: Element -> Effect ChartRef
foreign import initById :: String -> Effect ChartRef
foreign import showLoading :: ChartRef -> Effect Unit
foreign import hideLoading :: ChartRef -> Effect Unit
foreign import setOptions :: forall r. ChartRef -> {|r} -> Effect Unit
foreign import on :: forall r. ChartRef -> EventName -> Callback {|r} -> Effect Unit
foreign import onQuery :: forall r. ChartRef -> EventName -> EventQuery -> Callback {|r} -> Effect Unit
