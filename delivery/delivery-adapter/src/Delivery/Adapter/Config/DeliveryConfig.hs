module Delivery.Adapter.Config.DeliveryConfig
  ( fromEnv,
    DeliveryConfig (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import Envs.Env (FromEnv (..))
import System.Environment (lookupEnv)

data DeliveryConfig = DeliveryConfig
  { inPath :: String,
    prefix :: String,
    prefixLength :: Int
  }

instance FromEnv DeliveryConfig where
  fromEnv = do
    inPath' <- lookupEnv "IN_PATH"
    prefix' <- lookupEnv "IN_FILE_PREFIX"
    let ip = fromMaybe "/Users/will/Desktop/in" inPath'
        p = fromMaybe "in" prefix'
    return $ DeliveryConfig ip p (length p)
