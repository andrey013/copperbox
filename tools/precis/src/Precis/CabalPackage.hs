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
    Extension
  , extractPrecis
  , known_extensions

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

type Extension = String


extractPrecis :: FilePath -> [Extension] -> IO (Either CabalFileError CabalPrecis)
extractPrecis cabal_file exts = do
    exists <- doesFileExist cabal_file
    if exists then extractP cabal_file exts `onSuccessM` post
              else return $ Left ERR_CABAL_FILE_MISSING
  where
    post = return . nubSourceFiles


known_extensions :: [Extension]
known_extensions = ["hs", "lhs"]

extractP :: FilePath -> [String] -> IO (Either CabalFileError CabalPrecis)
extractP cabal_file_path exts =
    safeReadPackageDescription normal cabal_file_path `onSuccessM` sk
  where
    root_to_cabal = dropFileName cabal_file_path  
    sk gen_pkg = do { (expos,privs) <- getSourceFiles gen_pkg root_to_cabal exts
                    ; return $ CabalPrecis 
                                 { package_name          = getName gen_pkg
                                 , package_version       = getVersion gen_pkg
                                 , path_to_cabal_file    = cabal_file_path
                                 , exposed_modules       = expos
                                 , internal_modules      = privs
                                 }
                    }

type SafeGPD = Either CabalFileError GenericPackageDescription

safeReadPackageDescription :: Verbosity -> FilePath -> IO SafeGPD
safeReadPackageDescription verbo path = 
  catch (liftM Right $ readPackageDescription verbo path)
        (\e -> return $ Left $ ERR_CABAL_FILE_PARSE $ show e)

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
    lib_mods <- mapM (resolveLibrary root exts)    $ allLibraries   pkg_desc
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
    cp { exposed_modules = exs', internal_modules = ins' }
  where
    exs'  = nub exs
    ins'  = filter (not . (`elem` exs')) $ nub ins 

--------------------------------------------------------------------------------
-- General helper

ctfold :: (a -> b -> b) -> b -> (CondTree v c a) -> b
ctfold op initial node = foldr compfold x (condTreeComponents node)
  where
    x                           = condTreeData node `op` initial
    compfold (_,t1, Nothing) b  = ctfold op b t1
    compfold (_,t1, Just t2) b  = ctfold op (ctfold op b t1) t2
                         

