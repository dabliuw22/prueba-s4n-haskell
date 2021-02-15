module Location.Adapter.Config.LocationConfig
  ( LocationConfig (..),
    fromEnv,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import Envs.Env (FromEnv (..))
import System.Environment (lookupEnv)

data LocationConfig = LocationConfig
  { outPath :: String,
    prefix :: String,
    extension :: String
  }

instance FromEnv LocationConfig where
  fromEnv = do
    outPath' <- lookupEnv "OUT_PATH"
    prefix' <- lookupEnv "OUT_FILE_PREFIX"
    ext' <- lookupEnv "EXT_FILE_PREFIX"
    let op = fromMaybe "/Users/will/Desktop/out" outPath'
        p = fromMaybe "out" prefix'
        e = fromMaybe ".txt" ext'
    return $ LocationConfig op p e
