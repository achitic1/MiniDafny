{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_miniDafny (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/achitic/CMSC433/Assignments/FinalProject/MiniDafny/FinalProject/.stack-work/install/x86_64-linux-tinfo6/780f408425888d27d1550264489409a8117b5b9a3bbb3ed3d6f8cf8faafebf20/9.4.5/bin"
libdir     = "/home/achitic/CMSC433/Assignments/FinalProject/MiniDafny/FinalProject/.stack-work/install/x86_64-linux-tinfo6/780f408425888d27d1550264489409a8117b5b9a3bbb3ed3d6f8cf8faafebf20/9.4.5/lib/x86_64-linux-ghc-9.4.5/miniDafny-0.1.0.0-9IMdsoJkxyABj3qCm1OGSP-miniDafny"
dynlibdir  = "/home/achitic/CMSC433/Assignments/FinalProject/MiniDafny/FinalProject/.stack-work/install/x86_64-linux-tinfo6/780f408425888d27d1550264489409a8117b5b9a3bbb3ed3d6f8cf8faafebf20/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/achitic/CMSC433/Assignments/FinalProject/MiniDafny/FinalProject/.stack-work/install/x86_64-linux-tinfo6/780f408425888d27d1550264489409a8117b5b9a3bbb3ed3d6f8cf8faafebf20/9.4.5/share/x86_64-linux-ghc-9.4.5/miniDafny-0.1.0.0"
libexecdir = "/home/achitic/CMSC433/Assignments/FinalProject/MiniDafny/FinalProject/.stack-work/install/x86_64-linux-tinfo6/780f408425888d27d1550264489409a8117b5b9a3bbb3ed3d6f8cf8faafebf20/9.4.5/libexec/x86_64-linux-ghc-9.4.5/miniDafny-0.1.0.0"
sysconfdir = "/home/achitic/CMSC433/Assignments/FinalProject/MiniDafny/FinalProject/.stack-work/install/x86_64-linux-tinfo6/780f408425888d27d1550264489409a8117b5b9a3bbb3ed3d6f8cf8faafebf20/9.4.5/etc"

getBinDir     = catchIO (getEnv "miniDafny_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "miniDafny_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "miniDafny_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "miniDafny_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "miniDafny_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "miniDafny_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
