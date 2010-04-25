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
import Precis.Utils

import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version

import Control.Monad
import Data.List ( intersperse, nub )
import System.Directory
import System.FilePath

type CabalErr = String


extractPrecis :: FilePath -> [String] -> IO (Either CabalErr CabalPrecis)
extractPrecis cabal_file exts = 
    condM (doesFileExist cabal_file) (liftM post $ extractP cabal_file exts)
                                     (return $ "Missing cabal file - " ++ cabal_file)
  where
    post = rewriteModulePaths . nubSourceFiles


extractP :: FilePath -> [String] -> IO CabalPrecis
extractP cabal_file_path exts = do 
    gen_pkg  <- readPackageDescription normal cabal_file_path
    (expos,privs) <- getSourceFiles gen_pkg path_to_cabal exts
    return $ CabalPrecis 
               { cp_name               = getName gen_pkg
               , cp_version            = getVersion gen_pkg
               , cp_cabal_file         = cabal_file_path
               , cp_exposed_modules    = expos
               , cp_internal_modules   = privs
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



-- extract source files

getSourceFiles :: GenericPackageDescription 
           -> FilePath
           -> [String] 
           -> IO ([SourceFile], [SourceFile])
getSourceFiles pkg_desc root exts = do 
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


resolveLibrary :: FilePath -> [String] -> Library -> IO ([SourceFile], [SourceFile])
resolveLibrary root exts lib = liftM2 (,) (fn expos) (fn others)
  where
    fn mods                    = resolveFiles root src_paths mods exts
    (src_paths, expos, others) = libraryContents lib  

libraryContents :: Library -> ([FilePath], [ModuleName], [ModuleName])
libraryContents lib = (src_paths, expo_modules, other_modules)
  where
    src_paths       = hsSourceDirs   $ libBuildInfo lib
    expo_modules    = exposedModules lib
    other_modules   = otherModules   $ libBuildInfo lib

resolveExecutable :: FilePath -> [String] -> Executable -> IO [SourceFile]
resolveExecutable root exts exe = resolveFiles root src_paths mods exts
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

--------------------------------------------------------------------------------
-- reconcile modules

-- Initially a precis may have dublicates (via Cabal\'s 
-- conditional mechanism).
--
nubSourceFiles :: CabalPrecis -> CabalPrecis
nubSourceFiles cp@(CabalPrecis _ _ _ exs ins) = 
    cp { cp_exposed_modules = exs', cp_internal_modules = ins' }
  where
    exs'  = nub exs
    ins'  = filter (not . (`elem` exs')) $ nub ins 

rewriteModulePaths :: CabalPrecis -> CabalPrecis
rewriteModulePaths cp@(CabalPrecis _ _ root exs ins) = 
    cp { cp_exposed_modules = exs', cp_internal_modules = ins' }
  where
    exs'  = map (relativeModulePath root) exs
    ins'  = map (relativeModulePath root) ins

relativeModulePath :: FilePath -> SourceFile -> SourceFile
relativeModulePath path_to_cabal (SourceFile name path) = 
    SourceFile name (removePrefix (dropFileName path_to_cabal) path)
relativeModulePath _             modu                     = modu


--------------------------------------------------------------------------------
-- General helper

ctfold :: (a -> b -> b) -> b -> (CondTree v c a) -> b
ctfold op initial node = foldr compfold x (condTreeComponents node)
  where
    x                           = condTreeData node `op` initial
    compfold (_,t1, Nothing) b  = ctfold op b t1
    compfold (_,t1, Just t2) b  = ctfold op (ctfold op b t1) t2
                         

