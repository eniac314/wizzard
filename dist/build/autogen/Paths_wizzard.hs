module Paths_wizzard (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/bin"
libdir     = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.2/wizzard-0.1.0.0"
datadir    = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/share/x86_64-linux-ghc-7.10.2/wizzard-0.1.0.0"
libexecdir = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/libexec"
sysconfdir = "/home/fayong/prog/Haskell/wizzard/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wizzard_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wizzard_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wizzard_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wizzard_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wizzard_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
