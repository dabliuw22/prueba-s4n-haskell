{-# LANGUAGE TupleSections #-}

module Files.Interpreter (resourceInterpret, ResourceInterpreter (..)) where

import Control.Monad.Free (Free (..))
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (toUpper)
import Files.Dsl
  ( Directory (..),
    Extension (..),
    File (..),
    Filename (..),
    Line (..),
    ResourceF (..),
    ResourceScript,
  )
import Logs.Interpreter (LoggerInterpreter (..), loggerInterpret)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO
  ( FilePath,
    readFile,
    writeFile,
  )

class Monad m => ResourceInterpreter m where
  onGet :: Directory -> Extension -> m [File]
  onPut :: File -> Directory -> m ()
  onCreate :: Directory -> m FilePath

resourceInterpret ::
  ( Monad m,
    ResourceInterpreter m,
    LoggerInterpreter m
  ) =>
  ResourceScript a ->
  m a
resourceInterpret (Pure a) = return a
resourceInterpret (Free (Get l dir ext next)) = do
  _ <- loggerInterpret l
  v <- onGet dir ext
  resourceInterpret (next v)
resourceInterpret (Free (Put l file dir v)) = do
  _ <- loggerInterpret l
  onPut file dir
  resourceInterpret v
resourceInterpret (Free (Create l dir next)) = do
  _ <- loggerInterpret l
  v <- onCreate dir
  resourceInterpret (next v)

instance ResourceInterpreter IO where
  onGet (Directory dir') ext = do
    f <- listDirectory dir'
    let fp = (dir' </>) <$> clean f ext
    files <- readF fp
    return $ tupleToFile <$> files
    where
      clean :: [FilePath] -> Extension -> [FilePath]
      clean fp' All = fp'
      clean fp' (Custom ext) = filter (\a -> takeExtension a == ext) fp'
      readF :: [FilePath] -> IO [(FilePath, String)]
      readF [] = pure []
      readF fp = sequence $ (\fp' -> (fp',) <$> readFile fp') <$> fp
      tupleToFile :: (FilePath, String) -> File
      tupleToFile (fp, s) =
        File (Filename $ takeBaseName fp) (Line <$> lines s)
  onPut (File (Filename name') lines') (Directory dir') = do
    let filename = dir' </> name'
    writeFile filename (readLines lines')
    where
      readLines :: [Line] -> String
      readLines [] = ""
      readLines lines = line $ getLine lines
      getLine :: [Line] -> Line
      getLine lines = foldl (<>) (Line "") lines
  onCreate (Directory dir') = do
    _ <- remove dir'
    _ <- createDirectoryIfMissing True dir'
    pure dir'
    where
      remove :: FilePath -> IO ()
      remove dir = do
        exists <- doesDirectoryExist dir
        if exists
          then removeDirectoryRecursive dir
          else pure ()
