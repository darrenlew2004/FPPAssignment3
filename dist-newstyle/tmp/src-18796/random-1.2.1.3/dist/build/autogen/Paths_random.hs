{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_random (
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
version = Version [1,2,1,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\Downloads\\cabal\\store\\ghc-9.4.8\\random-1.2.1.3-39b1cb87ad056c8398d3cac8913e6411418e321d\\bin"
libdir     = "D:\\Downloads\\cabal\\store\\ghc-9.4.8\\random-1.2.1.3-39b1cb87ad056c8398d3cac8913e6411418e321d\\lib"
dynlibdir  = "D:\\Downloads\\cabal\\store\\ghc-9.4.8\\random-1.2.1.3-39b1cb87ad056c8398d3cac8913e6411418e321d\\lib"
datadir    = "D:\\Downloads\\cabal\\store\\ghc-9.4.8\\random-1.2.1.3-39b1cb87ad056c8398d3cac8913e6411418e321d\\share"
libexecdir = "D:\\Downloads\\cabal\\store\\ghc-9.4.8\\random-1.2.1.3-39b1cb87ad056c8398d3cac8913e6411418e321d\\libexec"
sysconfdir = "D:\\Downloads\\cabal\\store\\ghc-9.4.8\\random-1.2.1.3-39b1cb87ad056c8398d3cac8913e6411418e321d\\etc"

getBinDir     = catchIO (getEnv "random_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "random_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "random_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "random_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "random_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "random_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
