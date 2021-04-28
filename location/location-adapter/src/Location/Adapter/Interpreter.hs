module Location.Adapter.Interpreter where

import Control.Monad.Free (Free (..))
import Core.Drone (Drone)
import Files.Dsl (File)
import Files.Interpreter (ResourceInterpreter, resourceInterpret)
import Location.Domain.Dsl (LocationF (..), LocationScript)
import Logs.Interpreter (LoggerInterpreter, loggerInterpret)

class Monad m => LocationInterpreter m where
  onPut :: () -> m ()

instance LocationInterpreter IO where
  onPut r = return r

locationInterpret ::
  ( Monad m,
    ResourceInterpreter m,
    LoggerInterpreter m,
    LocationInterpreter m
  ) =>
  LocationScript a ->
  m a
locationInterpret (Pure a) = return a
locationInterpret (Free (Put l r next)) = do
  _ <- loggerInterpret l
  r <- resourceInterpret r
  v <- onPut r
  locationInterpret (next v)
