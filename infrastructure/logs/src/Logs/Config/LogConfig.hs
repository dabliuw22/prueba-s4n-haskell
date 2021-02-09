module Logs.Config.LogConfig (FromEnv (..), LogConfig (..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Envs.Env (FromEnv (..))
import System.Environment (lookupEnv)

data LogConfig = LogConfig
  { dir :: String,
    filename :: String,
    appName :: String,
    appEnv :: String
  }

instance FromEnv LogConfig where
  fromEnv = do
    dir' <- liftIO $ lookupEnv "LOGS_DIR"
    filename' <- liftIO $ lookupEnv "LOGS_FILENAME"
    appName' <- liftIO $ lookupEnv "APP_NAME"
    appEnv' <- liftIO $ lookupEnv "APP_ENV"
    return
      LogConfig
        { dir = fromMaybe "log" dir',
          filename = fromMaybe "prueba-s4n-haskell.log" filename',
          appName = fromMaybe "prueba-s4n-haskell" appName',
          appEnv = fromMaybe "local" appEnv'
        }
