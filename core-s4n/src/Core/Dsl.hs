{-# LANGUAGE TemplateHaskell #-}

module Core.Dsl
  ( Cmd (..),
    Coordinates (..),
    Direction (..),
    Drone (..),
    eval,
    makeCmd,
    Position (..),
    X (..),
    Y (..),
  )
where

import Control.Lens (Field1 (_1), makeLenses, makePrisms, (%~), (&), (?~))
import Data.Maybe (fromJust)

data Direction = North | South | East | West deriving (Show)

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

initPosition :: Position
initPosition = Position (Coordinates (X 0) (Y 0)) North

data Cmd
  = Init {next :: Cmd}
  | End {previous :: Maybe Position}
  | A {previous :: Maybe Position, next :: Cmd}
  | I {previous :: Maybe Position, next :: Cmd}
  | D {previous :: Maybe Position, next :: Cmd}

makePrisms ''Cmd

instance Show Cmd where
  show (Init cmd) = show cmd
  show (A _ cmd) = "A(" <> show cmd <> ")"
  show (I _ cmd) = "I(" <> show cmd <> ")"
  show (D _ cmd) = "D(" <> show cmd <> ")"
  show (End _) = ""

data Drone = Drone String [Cmd] deriving (Show)

makeCmd :: String -> Either String Cmd
makeCmd route = do
  t <- makeCmdR (reverse route) (End Nothing)
  return $ Init t
  where
    reverse :: [Char] -> String
    reverse xs = foldl (flip (:)) [] xs
    makeCmdR :: [Char] -> Cmd -> Either String Cmd
    makeCmdR [] cmd = Right cmd
    makeCmdR (h : t) cmd =
      case h of
        'A' -> makeCmdR t (A Nothing cmd)
        'I' -> makeCmdR t (I Nothing cmd)
        'D' -> makeCmdR t (D Nothing cmd)
        _ -> Left "Invalid Char Input" -- error "Invalid Char Input"

eval :: Cmd -> Position
eval (Init n) = eval (update n initPosition)
eval a@(A p n) = eval (update n (calculate a (fromJust p)))
eval i@(I p n) = eval (update n (calculate i (fromJust p)))
eval d@(D p n) = eval (update n (calculate d (fromJust p)))
eval (End p) = fromJust p

update :: Cmd -> Position -> Cmd
update a@(A _ n) p = a & (_A . _1) ?~ p -- (_A . _1) .~ (Just p) $ a
update i@(I _ n) p = i & (_I . _1) ?~ p
update d@(D _ n) p = d & (_D . _1) ?~ p
update end@(End _) p = end & (_End ?~ p)
update cmd _ = cmd

calculate :: Cmd -> Position -> Position
calculate cmd (Position c North) = fromNorth cmd c
calculate cmd (Position c South) = fromSouth cmd c
calculate cmd (Position c West) = fromWest cmd c
calculate cmd (Position c East) = fromEast cmd c

fromNorth :: Cmd -> Coordinates -> Position
fromNorth (A _ _) c = Position (c & (cy . y) %~ (+ 1)) North
fromNorth (I _ _) c = Position c East
fromNorth (D _ _) c = Position c West
fromNorth _ c = Position c North

fromSouth :: Cmd -> Coordinates -> Position
fromSouth (A _ _) c = Position (c & (cy . y) %~ (+ (-1))) South
fromSouth (I _ _) c = Position c West
fromSouth (D _ _) c = Position c East
fromSouth _ c = Position c South

fromWest :: Cmd -> Coordinates -> Position
fromWest (A _ _) c = Position (c & (cx . x) %~ (+ 1)) West
fromWest (I _ _) c = Position c North
fromWest (D _ _) c = Position c South
fromWest _ c = Position c West

fromEast :: Cmd -> Coordinates -> Position
fromEast (A _ _) c = Position (c & (cx . x) %~ (+ (-1))) East
fromEast (I _ _) c = Position c South
fromEast (D _ _) c = Position c North
fromEast _ c = Position c East
