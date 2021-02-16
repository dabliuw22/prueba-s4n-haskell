module Location.Adapter.Repository
  ( LocationRepository (..),
  )
where

import Core.Dsl
  ( Drone (..),
    Position (..),
    eval,
  )
import qualified Files.File as F
import Location.Adapter.Config.LocationConfig
  ( LocationConfig (..),
    fromEnv,
  )
import Location.Domain.Repository (LocationRepository (..))

instance LocationRepository IO where
  save (Drone n []) = pure ()
  save drone = do
    config <- fromEnv
    let dir = F.Directory $ outPath config
    F.write (droneToFile config drone) dir

droneToFile :: LocationConfig -> Drone -> F.File
droneToFile config (Drone n cmds) =
  F.File
    (F.Filename (prefix config <> n <> extension config))
    (F.Line . show . eval <$> cmds)
