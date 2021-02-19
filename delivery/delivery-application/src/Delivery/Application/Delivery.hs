module Delivery.Application.Delivery (DeliveryService (run)) where

import Core.Dsl (Drone)

class Monad m => DeliveryService m where
  run :: m [Drone] -> ([Drone] -> m ()) -> m ()

instance DeliveryService IO where
  -- run :: IO [Drone] -> ([Drone] -> IO ()) -> IO ()
  run drones f = drones >>= f
