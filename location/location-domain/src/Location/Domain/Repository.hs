module Location.Domain.Repository where

import Core.Dsl (Drone (..))

class Monad m => LocationRepository m where
  save :: Drone -> m ()
