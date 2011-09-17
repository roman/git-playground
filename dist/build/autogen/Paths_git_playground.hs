module Paths_git_playground (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/vagrant/haskell/Git/cabal-dev//bin"
libdir     = "/vagrant/haskell/Git/cabal-dev//lib/git-playground-0.1/ghc-7.0.3"
datadir    = "/vagrant/haskell/Git/cabal-dev//share/git-playground-0.1"
libexecdir = "/vagrant/haskell/Git/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "git_playground_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "git_playground_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "git_playground_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "git_playground_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
