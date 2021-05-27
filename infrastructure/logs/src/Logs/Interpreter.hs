{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Logs.Interpreter (loggerInterpret, LoggerInterpreter (..)) where

import Control.Exception (bracket)
import Control.Monad.Free (Free (..))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Katip
  ( ColorStrategy (ColorIfTerminal),
    LogEnv,
    Severity (DebugS, ErrorS, InfoS, WarningS),
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    logStr,
    logTM,
    mkFileScribe,
    mkHandleScribe,
    permitItem,
    registerScribe,
    runKatipContextT,
  )
import Logs.Config.LogConfig (FromEnv (..), LogConfig (..))
import Logs.Dsl
  ( LogCtx (..),
    LogLevel (..),
    LogMsg (..),
    LoggerF (..),
    LoggerScript,
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.IO (stdout)

class Monad m => LoggerInterpreter m where
  onLog :: LogLevel -> LogCtx -> LogMsg -> m ()

loggerInterpret :: (Monad m, LoggerInterpreter m) => LoggerScript a -> m a
loggerInterpret (Pure a) = return a
loggerInterpret (Free (Log level ctx msg v)) = do
  onLog level ctx msg
  loggerInterpret v

instance LoggerInterpreter IO where
  onLog level (LogCtx ctx) (LogMsg msg) = do
    envs <- fromEnv
    bracket (makeLogEnv envs) closeScribes (\env -> logAction env level ctx msg)
    where
      makeLogEnv :: LogConfig -> IO LogEnv
      makeLogEnv (LogConfig dir' filename' name env) = do
        let file = dir' <> "/" <> filename'
        logEnv <-
          initLogEnv
            (fromString name)
            (fromString env)
        _ <- createDirectoryIfMissing True dir'
        stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
        fileScribe <- mkFileScribe file (permitItem InfoS) V2
        newLogEnv <- registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
        registerScribe "file" fileScribe defaultScribeSettings newLogEnv
      logAction :: LogEnv -> LogLevel -> String -> String -> IO ()
      logAction env level ctx msg =
        runKatipContextT env () "" $ do
          $(logTM) (katipLevel level) $ logStr msg
      katipLevel :: LogLevel -> Severity
      katipLevel Info = InfoS
      katipLevel Error = ErrorS
      katipLevel Warning = WarningS
      katipLevel Debug = DebugS
