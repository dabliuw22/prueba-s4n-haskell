{-# LANGUAGE GADTs #-}

module Location.Application.Dsl where

import Control.Monad.Free (Free (..))
import Core.Drone (Drone)
import Location.Domain.Dsl (LocationScript)
import Logs.Dsl (LoggerScript)

data LocationA a where
  Put ::
    LoggerScript () ->
    LocationScript () ->
    (() -> a) ->
    LocationA a

instance Functor LocationA where
  fmap f (Put log res g) = Put log res (f . g)

type LocationAScript = Free LocationA

put ::
  LoggerScript () ->
  LocationScript () ->
  LocationAScript ()
put logs' loc' =
  Free $ Put logs' loc' pure
