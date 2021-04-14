{-# LANGUAGE GADTs #-}

module Logs.Dsl
  ( LoggerF (..),
    LoggerScript,
    LogLevel (..),
    LogCtx (..),
    LogMsg (..),
    mkInfo,
    mkError,
    mkDebug,
    mkWarning,
    mkLogCtx,
    mkLogMsg,
    logger,
  )
where

import Control.Monad.Free (Free (..), liftF)

data LogLevel = Info | Error | Debug | Warning

mkInfo :: LogLevel
mkInfo = Info

mkError :: LogLevel
mkError = Error

mkDebug :: LogLevel
mkDebug = Debug

mkWarning :: LogLevel
mkWarning = Warning

newtype LogCtx = LogCtx String

mkLogCtx :: String -> LogCtx
mkLogCtx = LogCtx

newtype LogMsg = LogMsg String

mkLogMsg :: String -> LogMsg
mkLogMsg = LogMsg

data LoggerF a where
  Log :: LogLevel -> LogCtx -> LogMsg -> a -> LoggerF a

instance Functor LoggerF where
  fmap f (Log level ctx msg a) = Log level ctx msg (f a)

type LoggerScript a = Free LoggerF a

logger :: LogLevel -> LogCtx -> LogMsg -> LoggerScript ()
logger level ctx msg = Free $ Log level ctx msg (pure ())
