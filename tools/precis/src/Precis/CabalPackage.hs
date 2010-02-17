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
    CabalErr
  , extractPrecis
  ) where

import Precis.Datatypes
import Precis.PathUtils

import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version

import Control.Monad
import Data.List (intersperse)
import System.FilePath

type CabalErr = String

extractPrecis :: FilePath -> [String] -> IO (Either CabalErr CabalPrecis)
extractPrecis cabal_file_path exts = do 
    gen_pkg  <- readPackageDescription normal cabal_file_path
    (expos,privs) <- getModules gen_pkg path_to_cabal exts
    return $ Right $ CabalPrecis 
                      { pkg_name              = getName gen_pkg
                      , pkg_version           = getVersion gen_pkg
                      , pkg_exposed_modules   = expos
                      , pkg_internal_modules  = privs
                      }
  where
    path_to_cabal = dropFileName cabal_file_path  

--------------------------------------------------------------------------------
-- Extract from Package description

getName    :: GenericPackageDescription -> String
getName    = extrNameText    . package . packageDescription 

getVersion :: GenericPackageDescription -> String
getVersion = extrVersionText . package . packageDescription 
                                  

extrNameText :: PackageIdentifier  -> String
extrNameText = fn . pkgName
  where fn (PackageName str) = str 

extrVersionText :: PackageIdentifier -> String
extrVersionText = fn . versionBranch . pkgVersion
  where fn = concat . intersperse "." . map show



-- extract modules

getModules :: GenericPackageDescription 
           -> FilePath
           -> [String] 
           -> IO ([SourceModule], [SourceModule])
getModules pkg_desc root exts = do 
    lib_mods <- mapM (resolveLibrary root exts) $ allLibraries pkg_desc
    exe_mods <- mapM (resolveExecutable root exts) $ allExecutables pkg_desc
    let (lib_expos, lib_privs) = foldr fn ([],[]) lib_mods 
    return (lib_expos, lib_privs ++ concat exe_mods)
  where 
    fn (a,b) (xs,ys) = (a++xs,b++ys)                                 


allLibraries :: GenericPackageDescription -> [Library]
allLibraries = maybe [] fn . condLibrary 
  where
   fn :: CondTree ConfVar [Dependency] Library -> [Library]
   fn = ctfold (:) []

 
allExecutables :: GenericPackageDescription -> [Executable]
allExecutables = concat . map (ctfold (:) [] . snd) . condExecutables


resolveLibrary :: FilePath -> [String] -> Library -> IO ([SourceModule], [SourceModule])
resolveLibrary root exts lib = liftM2 (,) (fn expos) (fn others)
  where
    fn mods                    = resolveModules root src_paths mods exts
    (src_paths, expos, others) = libraryContents lib  

libraryContents :: Library -> ([FilePath], [ModuleName], [ModuleName])
libraryContents lib = (src_paths, expo_modules, other_modules)
  where
    src_paths       = hsSourceDirs   $ libBuildInfo lib
    expo_modules    = exposedModules lib
    other_modules   = otherModules   $ libBuildInfo lib

resolveExecutable :: FilePath -> [String] -> Executable -> IO [SourceModule]
resolveExecutable root exts exe = resolveModules root src_paths mods exts
  where
    (src_paths, mods) = executableModules exe


executableContents :: Executable -> ([FilePath], FilePath, [ModuleName])
executableContents exe = (src_paths, exe_main_module, other_modules)
  where
    src_paths       = hsSourceDirs   $ buildInfo exe
    exe_main_module = modulePath exe
    other_modules   = otherModules   $ buildInfo exe


-- All executable modules considered internal...

executableModules :: Executable -> ([FilePath], [ModuleName])
executableModules = fn . executableContents 
  where
    fn (as,exe,cs) = (as, exeModuleName exe : cs)


-- General helper

ctfold :: (a -> b -> b) -> b -> (CondTree v c a) -> b
ctfold op initial node = foldr compfold x (condTreeComponents node)
  where
    x                           = condTreeData node `op` initial
    compfold (_,t1, Nothing) b  = ctfold op b t1
    compfold (_,t1, Just t2) b  = ctfold op (ctfold op b t1) t2
                         

