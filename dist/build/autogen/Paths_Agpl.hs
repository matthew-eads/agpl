module Paths_Agpl (
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

bindir     = "/home/kingcharles/compsci/150PLD/agpl/.cabal-sandbox/bin"
libdir     = "/home/kingcharles/compsci/150PLD/agpl/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/Agpl-0.1.0.0"
datadir    = "/home/kingcharles/compsci/150PLD/agpl/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/Agpl-0.1.0.0"
libexecdir = "/home/kingcharles/compsci/150PLD/agpl/.cabal-sandbox/libexec"
sysconfdir = "/home/kingcharles/compsci/150PLD/agpl/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Agpl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Agpl_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Agpl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Agpl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Agpl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
