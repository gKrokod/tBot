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

bindir     = "/home/w/projects/git/tBot/.stack-work/install/x86_64-linux/f73eb4554bca2dc4152cef32c3087104cd11b38475ed2068678f59ce79e1444a/9.0.2/bin"
libdir     = "/home/w/projects/git/tBot/.stack-work/install/x86_64-linux/f73eb4554bca2dc4152cef32c3087104cd11b38475ed2068678f59ce79e1444a/9.0.2/lib/x86_64-linux-ghc-9.0.2/bothttp-0.1.0.0-8CP1B7xD0JK2uuIfS7ooQb"
dynlibdir  = "/home/w/projects/git/tBot/.stack-work/install/x86_64-linux/f73eb4554bca2dc4152cef32c3087104cd11b38475ed2068678f59ce79e1444a/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/w/projects/git/tBot/.stack-work/install/x86_64-linux/f73eb4554bca2dc4152cef32c3087104cd11b38475ed2068678f59ce79e1444a/9.0.2/share/x86_64-linux-ghc-9.0.2/bothttp-0.1.0.0"
libexecdir = "/home/w/projects/git/tBot/.stack-work/install/x86_64-linux/f73eb4554bca2dc4152cef32c3087104cd11b38475ed2068678f59ce79e1444a/9.0.2/libexec/x86_64-linux-ghc-9.0.2/bothttp-0.1.0.0"
sysconfdir = "/home/w/projects/git/tBot/.stack-work/install/x86_64-linux/f73eb4554bca2dc4152cef32c3087104cd11b38475ed2068678f59ce79e1444a/9.0.2/etc"

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
