module Location.Application.Location (LocationService (save)) where

import Core.Dsl (Drone (..))

class Monad m => LocationService m where
  save :: (Drone -> m ()) -> [Drone] -> m ()

instance LocationService IO where
  -- save :: (Drone -> IO ()) -> [Drone] -> IO ()
  save f drones' = do
    sequence_ (f <$> drones')
