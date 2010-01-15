{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.CabalPackage
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.CabalPackage
  (
    CabalPrecis(..)
  , LibraryPrecis(..)
 
  , getCabalPrecis

  , readPackageDescr
  , readGenPackageDescr

  ) where

import Precis.Utils

import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version

import Control.Monad

data CabalPrecis = CabalPrecis {
        pkg_name                :: String,
        pkg_version             :: String,
        pkg_libraries           :: [LibraryPrecis]
      }
  deriving (Eq,Show)


data LibraryPrecis = LibraryPrecis {
        lib_hs_source_dirs      :: [FilePath],
        lib_exposed_modules     :: [FilePath],
        lib_hidden_modules      :: [FilePath]
      }
  deriving (Eq,Show)


readPackageDescr :: FilePath -> IO PackageDescription
readPackageDescr = liftM packageDescription . readPackageDescription normal

readGenPackageDescr :: FilePath -> IO GenericPackageDescription
readGenPackageDescr = readPackageDescription normal


getCabalPrecis :: FilePath -> IO CabalPrecis
getCabalPrecis = liftM extractPrecis . readPackageDescription normal 

extractPrecis :: GenericPackageDescription -> CabalPrecis
extractPrecis gp = 
    CabalPrecis 
      { pkg_name            = name
      , pkg_version         = version
      , pkg_libraries       = opt_expo_mod `mbCons` cond_expo_mods
      }
  where
    pkg             = packageDescription gp
    name            = extrPackageName             $ packageName gp
    version         = extrPackageVersion          $ packageVersion gp
    opt_expo_mod    = fmap extrLibraryPrecis      $ library pkg
    cond_expo_mods  = maybe [] sk $ condLibrary gp
    sk              = map extrLibraryPrecis . extrCondLibraries

extrLibraryPrecis :: Library -> LibraryPrecis
extrLibraryPrecis lib = 
    LibraryPrecis
      { lib_hs_source_dirs  = src_dirs
      , lib_exposed_modules = extrExposedModules lib
      , lib_hidden_modules  = hidden_mods
      }
  where
    src_dirs        = hsSourceDirs $ libBuildInfo lib
    hidden_mods     = map toFilePath $ otherModules $ libBuildInfo lib

--------------------------------------------------------------------------------

extrPackageName :: PackageName -> String
extrPackageName (PackageName str) = str


extrPackageVersion :: Version -> String
extrPackageVersion = para phi "" . versionBranch 
  where
    phi x ([],acc) = show x ++ acc
    phi x (_,acc)  = show x ++ ('.':acc)

extrExposedModules :: Library -> [FilePath]
extrExposedModules = map toFilePath . exposedModules 

extrCondLibraries :: CondTree ConfVar [Dependency] Library -> [Library]
extrCondLibraries = ctfold (:) [] where


ctfold :: (a -> b -> b) -> b -> (CondTree v c a) -> b
ctfold op initial node = foldr compfold x (condTreeComponents node)
  where
    x                           = condTreeData node `op` initial
    compfold (_,t1, Nothing) b  = ctfold op b t1
    compfold (_,t1, Just t2) b  = ctfold op (ctfold op b t1) t2
                         

