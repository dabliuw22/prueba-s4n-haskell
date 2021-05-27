{-# LANGUAGE GADTs #-}

module Delivery.Domain.Dsl
  ( DeliveryScript,
    DeliveryF (..),
    get,
  )
where

import Control.Monad.Free (Free (..))
import Core.Drone (Drone)
import Core.Fsl (Fsl (..))
import Core.Interpreter (eval)
import Files.Dsl (File, ResourceScript)
import Logs.Dsl (LoggerScript)

data DeliveryF a where
  Get ::
    LoggerScript () ->
    ResourceScript [File] ->
    ([File] -> [Drone]) ->
    ([Drone] -> a) ->
    DeliveryF a

instance Functor DeliveryF where
  fmap f (Get log' res' h g) = Get log' res' h (f . g)

type DeliveryScript = Free DeliveryF

get ::
  LoggerScript () ->
  ResourceScript [File] ->
  ([File] -> [Drone]) ->
  DeliveryScript [Drone]
get log' resource' f = do
  Free $ Get log' resource' f pure
