module Delivery.Domain.Configuration.Config
  ( DeliveryConfig (..),
    config,
  )
where

data DeliveryConfig = DeliveryConfig
  { inPath :: String,
    prefix :: String,
    extension :: String,
    prefixLength :: Int
  }

config :: String -> String -> String -> Int -> DeliveryConfig
config = DeliveryConfig
