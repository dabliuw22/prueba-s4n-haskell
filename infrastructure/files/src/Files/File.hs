{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Files.File where

import Conduit
  ( ConduitT,
    awaitForever,
    filterC,
    mapC,
    sourceDirectory,
    sourceFile,
    (.|),
  )
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (toUpper)
import Katip (Severity (InfoS), logStr, logTM, runKatipContextT)
import Logs.Log (logger)
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

newtype Directory = Directory String deriving (Show)

newtype Line = Line {line :: String} deriving (Show)

instance Semigroup Line where
  (Line x) <> (Line "") = Line x
  (Line "") <> (Line y) = Line y
  (Line x) <> (Line y) = Line (x <> "\n" <> y)

newtype Filename = Filename String deriving (Show)

data File = File Filename [Line] deriving (Show)

class Monad m => SimpleFileHandler m where
  create :: Directory -> m FilePath
  write :: File -> Directory -> m ()
  read :: Directory -> m [File]

class MonadResource m => StreamFileHandler m where
  readStream :: Directory -> ConduitT () File m ()

instance MonadResource m => StreamFileHandler m where
  readStream (Directory dir') =
    sourceDirectory dir'
      .| filterC (\a -> takeExtension a == ".txt")
      .| awaitForever transform
    where
      transform :: MonadResource m => FilePath -> ConduitT i File m ()
      transform fp =
        sourceFile fp
          .| mapC (\bs -> (fp, toUpper <$> unpack bs))
          .| mapC tupleToFile

tupleToFile :: (FilePath, String) -> File
tupleToFile (fp, s) =
  File (Filename $ takeBaseName fp) (Line <$> lines s)

instance SimpleFileHandler IO where
  create (Directory dir') = do
    _ <- logger $ \logEnv -> do
      runKatipContextT logEnv () "file-hanlder" $ do
        $(logTM) InfoS $ logStr ("Create Directory: " <> dir')
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
  write (File (Filename name') lines') (Directory dir') = do
    let filename = dir' </> name'
    _ <- logger $ \logEnv -> do
      runKatipContextT logEnv () "file-hanlder" $ do
        $(logTM) InfoS $ logStr ("Write File: " <> filename)
    writeFile filename (readLines lines')
    where
      readLines :: [Line] -> String
      readLines [] = ""
      readLines lines = line $ getLine lines
      getLine :: [Line] -> Line
      getLine lines = foldl (<>) (Line "") lines
  read (Directory dir') = do
    _ <- logger $ \logEnv -> do
      runKatipContextT logEnv () "file-hanlder" $ do
        $(logTM) InfoS $ logStr ("Read Directory: " <> dir')
    f <- listDirectory dir'
    let fp = (dir' </>) <$> filter (\a -> takeExtension a == ".txt") f
    files <- readF fp
    return $ tupleToFile <$> files
    where
      readF :: [FilePath] -> IO [(FilePath, String)]
      readF [] = pure []
      readF fp = sequence $ (\fp' -> (fp',) <$> readFile fp') <$> fp
