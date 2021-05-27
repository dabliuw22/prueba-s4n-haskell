module Env where

import Delivery.Adapter.Config.DeliveryConfig (DeliveryConfig)
import Location.Adapter.Config.LocationConfig (LocationConfig)
import Logs.Config.LogConfig (LogConfig)

data Env = Env
  { logs :: LogConfig,
    location :: LocationConfig,
    delivery :: DeliveryConfig
  }
