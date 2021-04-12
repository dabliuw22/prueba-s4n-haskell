module Core.Service where

import Core.Drone (Drone, Position, drone)
import Core.Fsl (Fsl)
import Core.Interpreter (eval)

data CoreHandle = CoreHandle
  { mkPosition :: Fsl () -> Position,
    mkDrone :: String -> [Position] -> Drone
  }

defaultCoreHandle :: CoreHandle
defaultCoreHandle = CoreHandle eval drone
