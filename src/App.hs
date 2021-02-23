{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module App (app) where

import qualified Delivery.Adapter.Repository as DAD
import qualified Delivery.Application.Delivery as DAP
import qualified Files.File as F
import Katip (Severity (InfoS), logTM, runKatipContextT)
import Location.Adapter.Config.LocationConfig
  ( LocationConfig (..),
    fromEnv,
  )
import qualified Location.Adapter.Repository as LAD
import qualified Location.Application.Location as LAP
import Logs.Log (logger)

app :: IO ()
app = do
  logger $ \logEnv -> do
    runKatipContextT logEnv () "app" $ do
      $(logTM) InfoS "Start App..."
  config <- fromEnv
  _ <- F.create $ F.Directory (outPath config)
  let f1 = LAP.save LAD.save
      f2 = DAD.findAll
      f3 = DAP.run f2 f1
  _ <- f3
  logger $ \logEnv -> do
    runKatipContextT logEnv () "app" $ do
      $(logTM) InfoS "End App..."
