{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Cabal.InterimDatatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatype for working with Cabal files...
--
--------------------------------------------------------------------------------


module Precis.Cabal.InterimDatatypes
  (

    FileExtension

  , CabalFilePath 
  , cabalFilePath
  , pathToCabalFile
  , directoriesToCabalFile

  , ExeMainPath(..)
  , CabalSourceDir
  , cabalSourceDir
  , directoriesToSource

  , ModName
  , modName
  , getModName
  
  , ModuleDesc
  , moduleDesc  
  , moduleDescName
  , moduleDirectories

  , CabalPrecis(..)
  , CabalLibrary(..)
  , CabalExe(..)

  ) where

import qualified Distribution.ModuleName        as D

import Data.List ( intersperse )
import System.FilePath



type FileExtension = String


-- | 'CabalFilePath' is both the full, normalized path to the 
-- cabal file and the directory parts to the file.
--
-- This is an opaque type - construct with 'cabalFilePath'.
-- 
data CabalFilePath = CabalFilePath 
      { cabal_full_loc        :: FilePath
      , cabal_path_to_split   :: [FilePath]
      }
  deriving (Eq,Ord,Show) 

-- | Constructor for 'CabalFilePath' - the input FilePath is 
-- normalized before constructing the data type.
--
cabalFilePath :: FilePath -> CabalFilePath
cabalFilePath path = 
    CabalFilePath { cabal_full_loc = full, cabal_path_to_split = parts }
  where
    full  = normalise path
    parts = splitPath $ dropFileName full
  
pathToCabalFile :: CabalFilePath -> FilePath
pathToCabalFile = cabal_full_loc

directoriesToCabalFile :: CabalFilePath -> [FilePath]
directoriesToCabalFile = cabal_path_to_split


newtype ExeMainPath = ExeMainPath { relPathToExeMain :: FilePath }
  deriving (Eq,Ord,Show)

data CabalSourceDir = CabalSourceDir
      { srcdir_rel_loc          :: FilePath
      , srcdir_path_to_split    :: [FilePath]
      }
  deriving (Eq,Ord,Show) 

cabalSourceDir :: FilePath -> CabalSourceDir
cabalSourceDir path = 
    CabalSourceDir { srcdir_rel_loc = rel, srcdir_path_to_split = parts }
  where
    rel   = normalise path
    parts = splitPath rel


directoriesToSource :: CabalSourceDir -> [FilePath]
directoriesToSource = srcdir_path_to_split


newtype ModName = ModName { mod_name :: String }
  deriving (Eq,Ord,Show)

modName :: D.ModuleName -> ModName
modName = ModName . concat . intersperse "."  . D.components

getModName :: ModName -> String
getModName = mod_name


data ModuleDesc = ModuleDesc 
      { module_desc_name    :: ModName
      , module_components   :: [FilePath]
      }
  deriving (Eq,Ord,Show)


moduleDesc :: D.ModuleName -> ModuleDesc
moduleDesc mname = 
    ModuleDesc { module_desc_name = name, module_components = parts }
  where
    xs    = D.components mname
    name  = ModName $ concat $ intersperse "." xs

    -- Add separator to inits but not last...
    parts = foldr fn [] xs where fn e [] = [e]
                                 fn e ac = (e++[pathSeparator]):ac
   

moduleDescName :: ModuleDesc -> ModName
moduleDescName = module_desc_name

moduleDirectories :: ModuleDesc -> [FilePath]
moduleDirectories = module_components

-- Do we want /resolution/ during the building 
-- the CabalPrecis or afterwards?
-- 
-- i.e. when do we query the file system to locate the modules? 


-- Because of CondTree/Conditional a Cabal file can appear as
-- though it contains more than one Library, some normalization
-- has to be performed on this structure...
--
data CabalPrecis = CabalPrecis
      { pkg_name              :: String
      , pkg_version           :: String
      , path_to_cabal_file    :: CabalFilePath
      , cond_libraries        :: [CabalLibrary]
      , cond_exes             :: [CabalExe]
      }
  deriving (Eq,Show)

-- One library per cabal file / package.

data CabalLibrary = CabalLibrary 
      { library_src_dirs    :: [CabalSourceDir]
      , public_modules      :: [ModuleDesc]
      , private_modules     :: [ModuleDesc]
      }
  deriving (Eq,Ord,Show)    

-- Zero / one or more exe per cabal file / package.
-- The executable file will include the extension...

data CabalExe = CabalExe
      { exe_main_module     :: ExeMainPath
      , exe_src_dirs        :: [CabalSourceDir]
      , exe_other_modules   :: [ModuleDesc] 
      }
  deriving (Eq,Ord,Show)





