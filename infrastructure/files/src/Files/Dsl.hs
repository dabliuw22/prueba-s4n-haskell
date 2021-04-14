module Files.Dsl
  ( ResourceF (..),
    ResourceScript,
    Directory (..),
    Extension (..),
    Filename (..),
    File (..),
    Line (..),
    create,
    get,
    put,
    mkAll,
    mkCustom,
    mkDirectory,
    mkFile,
    mkFilename,
    mkLine,
  )
where

import Control.Monad.Free (Free (..))
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (toUpper)
import Logs.Dsl (LoggerScript)
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

mkDirectory :: String -> Directory
mkDirectory = Directory

newtype Line = Line {line :: String} deriving (Show)

mkLine :: String -> Line
mkLine = Line

instance Semigroup Line where
  (Line x) <> (Line "") = Line x
  (Line "") <> (Line y) = Line y
  (Line x) <> (Line y) = Line (x <> "\n" <> y)

data Extension
  = All
  | Custom String
  deriving (Show, Eq)

mkAll :: Extension
mkAll = All

mkCustom :: String -> Extension
mkCustom = Custom

newtype Filename = Filename String deriving (Show)

mkFilename :: String -> Filename
mkFilename = Filename

data File = File Filename [Line] deriving (Show)

mkFile :: Filename -> [Line] -> File
mkFile = File

data ResourceF a
  = Get (LoggerScript ()) Directory Extension ([File] -> a)
  | Put (LoggerScript ()) File Directory a
  | Create (LoggerScript ()) Directory (FilePath -> a)

instance Functor ResourceF where
  fmap f (Get log dir ext g) = Get log dir ext (f . g)
  fmap f (Put log file dir a) = Put log file dir (f a)
  fmap f (Create log dir g) = Create log dir (f . g)

type ResourceScript a = Free ResourceF a

get ::
  LoggerScript () ->
  Directory ->
  Extension ->
  ResourceScript [File]
get log dir ext = Free $ Get log dir ext pure

put :: LoggerScript () -> File -> Directory -> ResourceScript ()
put log file dir = Free $ Put log file dir (pure ())

create :: LoggerScript () -> Directory -> ResourceScript FilePath
create log dir = Free $ Create log dir pure
