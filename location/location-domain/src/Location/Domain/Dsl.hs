{-# LANGUAGE GADTs #-}

module Location.Domain.Dsl
  ( LocationScript,
    LocationF (..),
    put,
  )
where

import Control.Monad.Free (Free (..))
import Core.Drone (Drone)
import Files.Dsl (File, ResourceScript)
import Logs.Dsl (LoggerScript)

data LocationF a where
  Put ::
    LoggerScript () ->
    ResourceScript () ->
    (() -> a) ->
    LocationF a

instance Functor LocationF where
  fmap f (Put log res g) = Put log res (f . g)

type LocationScript a = Free LocationF a

put ::
  LoggerScript () ->
  ResourceScript () ->
  LocationScript ()
put logs' resource' =
  Free $ Put logs' resource' pure
