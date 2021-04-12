{-# LANGUAGE TemplateHaskell #-}

module Core.Drone
  ( Drone,
    drone,
    origin,
    Position (..),
    Direction (..),
    Coordinates,
    north,
    south,
    east,
    west,
    cx,
    cy,
    x,
    y,
  )
where

import Control.Lens
  ( makeLenses,
    makePrisms,
  )
import Data.Maybe (fromJust)

data Direction = North | South | East | West deriving (Show)

north :: Direction
north = North

south :: Direction
south = South

east :: Direction
east = East

west :: Direction
west = West

newtype X = X {_x :: Int}

makeLenses ''X

instance Show X where
  show (X x) = show x

newtype Y = Y {_y :: Int}

makeLenses ''Y

instance Show Y where
  show (Y y) = show y

data Coordinates = Coordinates {_cx :: X, _cy :: Y}

makeLenses ''Coordinates

instance Show Coordinates where
  show (Coordinates x y) =
    "(" <> show x <> ", " <> show y <> ")"

data Position = Position {_coord :: Coordinates, _dir :: Direction}

makeLenses ''Position

instance Show Position where
  show (Position c d) = show c <> " " <> show d

origin :: Position
origin = Position (Coordinates (X 0) (Y 0)) North

data Drone = Drone String [Position] deriving (Show)

drone :: String -> [Position] -> Drone
drone = Drone
