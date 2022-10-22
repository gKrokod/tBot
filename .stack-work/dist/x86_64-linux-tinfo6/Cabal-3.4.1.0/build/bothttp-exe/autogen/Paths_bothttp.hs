{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_bothttp (
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

bindir     = "/home/h/project/git/tBot/.stack-work/install/x86_64-linux-tinfo6/f75d4542b39100484d295c5919b5bc0c488141cd14ff651a8876d006c83e98f0/9.0.2/bin"
libdir     = "/home/h/project/git/tBot/.stack-work/install/x86_64-linux-tinfo6/f75d4542b39100484d295c5919b5bc0c488141cd14ff651a8876d006c83e98f0/9.0.2/lib/x86_64-linux-ghc-9.0.2/bothttp-0.1.0.0-JkvNa7SVgfWCX52XP2fEa5-bothttp-exe"
dynlibdir  = "/home/h/project/git/tBot/.stack-work/install/x86_64-linux-tinfo6/f75d4542b39100484d295c5919b5bc0c488141cd14ff651a8876d006c83e98f0/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/h/project/git/tBot/.stack-work/install/x86_64-linux-tinfo6/f75d4542b39100484d295c5919b5bc0c488141cd14ff651a8876d006c83e98f0/9.0.2/share/x86_64-linux-ghc-9.0.2/bothttp-0.1.0.0"
libexecdir = "/home/h/project/git/tBot/.stack-work/install/x86_64-linux-tinfo6/f75d4542b39100484d295c5919b5bc0c488141cd14ff651a8876d006c83e98f0/9.0.2/libexec/x86_64-linux-ghc-9.0.2/bothttp-0.1.0.0"
sysconfdir = "/home/h/project/git/tBot/.stack-work/install/x86_64-linux-tinfo6/f75d4542b39100484d295c5919b5bc0c488141cd14ff651a8876d006c83e98f0/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bothttp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bothttp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bothttp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bothttp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bothttp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bothttp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
