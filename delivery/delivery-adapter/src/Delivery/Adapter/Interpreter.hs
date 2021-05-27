module Delivery.Adapter.Interpreter (DeliveryInterpreter (..), deliveryInterpret) where

import Control.Monad.Free (Free (..))
import Core.Drone (Drone)
import Delivery.Domain.Dsl (DeliveryF (..), DeliveryScript)
import Files.Dsl (File)
import Files.Interpreter (ResourceInterpreter, resourceInterpret)
import Logs.Interpreter (LoggerInterpreter, loggerInterpret)

class Monad m => DeliveryInterpreter m where
  onGet :: [File] -> ([File] -> [Drone]) -> m [Drone]

instance DeliveryInterpreter IO where
  onGet files' f = return $ f files'

deliveryInterpret ::
  ( Monad m,
    ResourceInterpreter m,
    LoggerInterpreter m,
    DeliveryInterpreter m
  ) =>
  DeliveryScript a ->
  m a
deliveryInterpret (Pure a) = return a
deliveryInterpret (Free (Get l r f next)) = do
  _ <- loggerInterpret l
  files' <- resourceInterpret r
  v <- onGet files' f
  deliveryInterpret (next v)
