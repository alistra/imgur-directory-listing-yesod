module Paths_imgur_directory_listing (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/alistra/.cabal/bin"
libdir     = "/home/alistra/.cabal/lib/imgur-directory-listing-0.0.0/ghc-7.0.4"
datadir    = "/home/alistra/.cabal/share/imgur-directory-listing-0.0.0"
libexecdir = "/home/alistra/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "imgur_directory_listing_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "imgur_directory_listing_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "imgur_directory_listing_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "imgur_directory_listing_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
