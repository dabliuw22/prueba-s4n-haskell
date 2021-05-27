module Core.Interpreter (eval) where

import Control.Lens
  ( Field1 (_1),
    (%~),
    (&),
    (?~),
  )
import Control.Monad.Free (Free (..))
import Core.Drone
  ( Coordinates,
    Direction (East, North, South, West),
    Position (Position),
    cx,
    cy,
    east,
    north,
    origin,
    south,
    west,
    x,
    y,
  )
import qualified Core.Dsl as Dsl
import qualified Core.Fsl as Fsl

eval :: Fsl.Fsl () -> Position
eval = cslInterpreter origin

cslInterpreter :: Position -> Fsl.Fsl () -> Position
cslInterpreter previous (Pure _) = previous
cslInterpreter previous (Free next@(Fsl.A csl)) =
  cslInterpreter (cmdInterpreter previous next) csl
cslInterpreter previous (Free next@(Fsl.I csl)) =
  cslInterpreter (cmdInterpreter previous next) csl
cslInterpreter previous (Free next@(Fsl.D csl)) =
  cslInterpreter (cmdInterpreter previous next) csl

cmdInterpreter :: Position -> Fsl.Cmd (Fsl.Fsl ()) -> Position
cmdInterpreter (Position c North) cmd = fromNorth cmd c
cmdInterpreter (Position c South) cmd = fromSouth cmd c
cmdInterpreter (Position c West) cmd = fromWest cmd c
cmdInterpreter (Position c East) cmd = fromEast cmd c

fromNorth :: Fsl.Cmd (Fsl.Fsl ()) -> Coordinates -> Position
fromNorth (Fsl.A csl) c = Position (c & (cy . y) %~ (+ 1)) north
fromNorth (Fsl.I csl) c = Position c east
fromNorth (Fsl.D csl) c = Position c west

fromSouth :: Fsl.Cmd (Fsl.Fsl ()) -> Coordinates -> Position
fromSouth (Fsl.A csl) c = Position (c & (cy . y) %~ (+ (-1))) south
fromSouth (Fsl.I csl) c = Position c west
fromSouth (Fsl.D csl) c = Position c east

fromWest :: Fsl.Cmd (Fsl.Fsl ()) -> Coordinates -> Position
fromWest (Fsl.A csl) c = Position (c & (cx . x) %~ (+ 1)) west
fromWest (Fsl.I csl) c = Position c north
fromWest (Fsl.D csl) c = Position c south

fromEast :: Fsl.Cmd (Fsl.Fsl ()) -> Coordinates -> Position
fromEast (Fsl.A csl) c = Position (c & (cx . x) %~ (+ (-1))) east
fromEast (Fsl.I csl) c = Position c south
fromEast (Fsl.D csl) c = Position c north
