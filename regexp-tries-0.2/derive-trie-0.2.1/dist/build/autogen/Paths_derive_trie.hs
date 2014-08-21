module Paths_derive_trie (
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
version = Version {versionBranch = [0,2,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/x86_64-linux-ghc-7.8.3/derive-trie-0.2.1"
datadir    = "/root/.cabal/share/x86_64-linux-ghc-7.8.3/derive-trie-0.2.1"
libexecdir = "/root/.cabal/libexec"
sysconfdir = "/root/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "derive_trie_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "derive_trie_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "derive_trie_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "derive_trie_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "derive_trie_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
