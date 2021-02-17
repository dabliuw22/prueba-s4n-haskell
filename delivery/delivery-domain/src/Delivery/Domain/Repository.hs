module Delivery.Domain.Repository (DeliveryRepository (..)) where

import Core.Dsl (Drone (..))

class Monad m => DeliveryRepository m where
  findAll :: m [Drone]
