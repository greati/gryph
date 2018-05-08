{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_igryph (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/vitorgreati/.cabal/bin"
libdir     = "/home/vitorgreati/.cabal/lib/x86_64-linux-ghc-8.2.2/igryph-0.1.0.0-D3sfEASfLaL374CQqV0qV0-igryph"
dynlibdir  = "/home/vitorgreati/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/vitorgreati/.cabal/share/x86_64-linux-ghc-8.2.2/igryph-0.1.0.0"
libexecdir = "/home/vitorgreati/.cabal/libexec/x86_64-linux-ghc-8.2.2/igryph-0.1.0.0"
sysconfdir = "/home/vitorgreati/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "igryph_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "igryph_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "igryph_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "igryph_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "igryph_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "igryph_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
