{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Delivery.Adapter.Repository
  ( DeliveryRepository (findAll),
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Core.Dsl
  ( Cmd,
    Drone (..),
    makeCmd,
  )
import Data.String.Utils (replace)
import Delivery.Adapter.Config.DeliveryConfig
  ( DeliveryConfig (..),
    fromEnv,
  )
import Delivery.Domain.Repository (DeliveryRepository (..))
import qualified Files.File as F
import Katip
  ( Severity (InfoS),
    logStr,
    logTM,
    runKatipContextT,
  )
import Logs.Log (logger)

instance DeliveryRepository IO where
  findAll = do
    _ <- logger $ \logEnv -> do
      runKatipContextT logEnv () "delivery" $ do
        $(logTM) InfoS "findAll"
    config <- fromEnv
    files <- F.read (F.Directory $ inPath config)
    mapConcurrently (fileToDrone (prefix config)) files

fileToDrone :: String -> F.File -> IO Drone
fileToDrone prefix' (F.File (F.Filename n) []) =
  pure $ Drone (replace prefix' "" n) []
fileToDrone prefix' (F.File (F.Filename n) lines) =
  pure $ Drone (replace prefix' "" n) (getCmds lines)

getCmds :: [F.Line] -> [Cmd]
getCmds [] = []
getCmds l = convertCmds $ makeCmd . F.line <$> l

convertCmds :: [Either String Cmd] -> [Cmd]
convertCmds [] = []
convertCmds cmds = do
  let l = sequence cmds
  case l of
    Right v -> v
    _ -> []
