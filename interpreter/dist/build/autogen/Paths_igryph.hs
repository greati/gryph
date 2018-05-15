module Paths_igryph (
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

bindir     = "/home/vitorgreati/.cabal/bin"
libdir     = "/home/vitorgreati/.cabal/lib/x86_64-linux-ghc-7.10.3/igryph-0.1.0.0-DhCiLDRHDlB1mzVnr1A05L"
datadir    = "/home/vitorgreati/.cabal/share/x86_64-linux-ghc-7.10.3/igryph-0.1.0.0"
libexecdir = "/home/vitorgreati/.cabal/libexec"
sysconfdir = "/home/vitorgreati/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "igryph_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "igryph_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "igryph_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "igryph_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "igryph_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
