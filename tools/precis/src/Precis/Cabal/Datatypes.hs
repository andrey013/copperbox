{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Cabal.Datatypes
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


module Precis.Cabal.Datatypes
  (

    CabalFilePath 
  , cabalFilePath
  , pathToCabalFile
  , directoriesToCabalFile

  , ExeMainPath(..)
  , CabalSourceDir
  , cabalSourceDir
  , directoriesToSource
  
  , ModuleDesc
  , moduleDesc  
  , moduleDescName
  , moduleDirectories

  , ModName

  , CabalFileError(..)
  , cabalFileErrorMsg

  , CabalPrecis(..)
  , CabalLibrary(..)
  , CabalExe(..)



  , SourceFile(..)
  , sourceFile 
  , UnresolvedModule(..)
  , componentName


  ) where

import qualified Distribution.ModuleName        as D

import Data.List ( intersperse )
import System.FilePath

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
    parts = splitPath $ dropFileName rel


directoriesToSource :: CabalSourceDir -> [FilePath]
directoriesToSource = srcdir_path_to_split


data ModuleDesc = ModuleDesc 
      { module_desc_name    :: String
      , module_components   :: [FilePath]
      }
  deriving (Eq,Ord,Show)


moduleDesc :: D.ModuleName -> ModuleDesc
moduleDesc mname = 
    ModuleDesc { module_desc_name = name, module_components = parts }
  where
    xs    = D.components mname
    name  = concat $ intersperse "." xs
    parts = map (++[pathSeparator]) xs

moduleDescName :: ModuleDesc -> String
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
      { package_name            :: String
      , package_version         :: String
      , path_to_cabal_file      :: CabalFilePath
      , cond_libraries          :: [CabalLibrary]
      , cond_exes               :: [CabalExe]
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





--------------------------------------------------------------------------------


data CabalFileError = ERR_CABAL_FILE_MISSING FilePath
                    | ERR_CABAL_FILE_PARSE   String
  deriving (Eq,Show)


cabalFileErrorMsg :: CabalFileError -> String
cabalFileErrorMsg (ERR_CABAL_FILE_MISSING s) = "*** Error: missing file - " ++ s
cabalFileErrorMsg (ERR_CABAL_FILE_PARSE   s) = "*** Error: parse error - " ++ s

-----

 
data SourceFile = SourceFile     
      { module_name            :: String
      , full_path_to           :: FilePath 
      }
  deriving (Eq,Ord,Show)

type ModName = D.ModuleName
newtype UnresolvedModule = UnresolvedModule { unresolved_name :: ModName }
  deriving (Eq,Ord,Show)


-- smart constructor

sourceFile :: ModName -> FilePath -> SourceFile
sourceFile name path = SourceFile (componentName name) (normalise path)

-- "A.B.C"
componentName :: ModName -> String
componentName = concat . intersperse "." . D.components

