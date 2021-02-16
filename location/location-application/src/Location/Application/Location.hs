module Location.Application.Location (save) where

import Core.Dsl (Drone (..))

save :: (Drone -> IO ()) -> [Drone] -> IO ()
save f drones' = do
  sequence_ (f <$> drones')
