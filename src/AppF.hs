{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module AppF where

import Control.Monad.Free (Free (..), foldFree)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Core.Drone (Drone, Position (..), drone, origin)
import qualified Core.Fsl as Core
import qualified Core.Interpreter as CI
import qualified Delivery.Adapter.Interpreter as DI
import qualified Delivery.Domain.Configuration.Config as DC
import qualified Delivery.Domain.Dsl as Dsl
import qualified Files.Dsl as Fsl
import qualified Files.Interpreter as FI
import qualified Logs.Dsl as Lsl
import qualified Logs.Interpreter as LI
import System.IO (FilePath)

--data AppM a = AppM { runAppM :: ReaderT Env IO a }

data AppF a where
  EvalLogger :: Lsl.LoggerScript () -> (() -> a) -> AppF a
  GetFiles :: Fsl.ResourceScript [Fsl.File] -> ([Fsl.File] -> a) -> AppF a
  WriteFile :: Fsl.ResourceScript () -> (() -> a) -> AppF a
  CreateDirectory :: Fsl.ResourceScript FilePath -> (FilePath -> a) -> AppF a
  BasicProgram :: Dsl.DeliveryScript [Drone] -> ([Drone] -> a) -> AppF a

instance Functor AppF where
  fmap f (EvalLogger l g) = EvalLogger l (f . g)
  fmap f (GetFiles r g) = GetFiles r (f . g)
  fmap f (WriteFile r g) = WriteFile r (f . g)
  fmap f (CreateDirectory r g) = CreateDirectory r (f . g)
  fmap f (BasicProgram d g) = BasicProgram d (f . g)

type AppScript a = Free AppF a

evalLogger :: String -> AppScript ()
evalLogger msj = Free $ EvalLogger (Lsl.logger Lsl.mkInfo (Lsl.mkLogCtx "Example") (Lsl.mkLogMsg msj)) pure

getFiles :: String -> Fsl.Directory -> Fsl.Extension -> AppScript [Fsl.File]
getFiles msj dir ext = do
  let loggerS = Lsl.logger Lsl.mkInfo (Lsl.mkLogCtx "Example") (Lsl.mkLogMsg msj)
  Free $ GetFiles (Fsl.get loggerS dir ext) pure

writeFile :: String -> Fsl.File -> Fsl.Directory -> AppScript ()
writeFile msj file dir = do
  let loggerS = Lsl.logger Lsl.mkInfo (Lsl.mkLogCtx "Example") (Lsl.mkLogMsg msj)
  Free $ WriteFile (Fsl.put loggerS file dir) pure

createDirectory :: String -> Fsl.Directory -> AppScript FilePath
createDirectory msj dir = do
  let loggerS = Lsl.logger Lsl.mkInfo (Lsl.mkLogCtx "Example") (Lsl.mkLogMsg msj)
  Free $ CreateDirectory (Fsl.create loggerS dir) pure

basicProgram :: String -> DC.DeliveryConfig -> AppScript [Drone]
basicProgram msj config' = do
  let loggerS = Lsl.logger Lsl.mkInfo (Lsl.mkLogCtx "Example") (Lsl.mkLogMsg msj)
      directory' = Fsl.mkDirectory (DC.inPath config')
      extension' = Fsl.mkCustom (DC.extension config')
      resourceS = Fsl.get loggerS directory' extension'
  Free $ BasicProgram (Dsl.get loggerS resourceS filesToDrones) pure

filesToDrones :: [Fsl.File] -> [Drone]
filesToDrones [] = []
filesToDrones files' = fileToDrone <$> files'

fileToDrone :: Fsl.File -> Drone
fileToDrone (Fsl.File (Fsl.Filename name) lines') =
  drone name (toPosition lines')
  where
    toPosition :: [Fsl.Line] -> [Position]
    toPosition [] = []
    toPosition lines' = lineToPosition <$> lines'
    lineToPosition :: Fsl.Line -> Position
    lineToPosition (Fsl.Line line') = makePosition line'
    makePosition :: String -> Position
    makePosition v =
      let fsl = Core.makeFsl v
       in case fsl of
            Right s -> CI.eval s
            _ -> origin

interpret :: forall a. AppF a -> IO a
interpret (EvalLogger l next) = next <$> LI.loggerInterpret l
interpret (GetFiles r next) = next <$> FI.resourceInterpret r
interpret (WriteFile r next) = next <$> FI.resourceInterpret r
interpret (CreateDirectory r next) = next <$> FI.resourceInterpret r
interpret (BasicProgram d next) = next <$> DI.deliveryInterpret d

runApp :: AppScript a -> IO a
runApp = foldFree interpret
