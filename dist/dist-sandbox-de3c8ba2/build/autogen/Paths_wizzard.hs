module Paths_wizzard (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/bin"
libdir     = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/wizzard-0.1.0.0"
datadir    = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/wizzard-0.1.0.0"
libexecdir = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "wizzard_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wizzard_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wizzard_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wizzard_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
