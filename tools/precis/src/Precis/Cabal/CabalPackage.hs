{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Cabal.CabalPackage
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.Cabal.CabalPackage 
  ( 
   
    extractPrecis
  , known_extensions

  ) where

import Precis.Cabal.Datatypes
import Precis.Cabal.PathUtils
import Precis.Utils.Common

import qualified Distribution.Package                   as D
import qualified Distribution.PackageDescription        as D
import qualified Distribution.PackageDescription.Parse  as D
import qualified Distribution.Verbosity                 as D
import qualified Distribution.Version                   as D

import Control.Monad
import Data.List ( intersperse )
import System.Directory


-- | File extensions that Precis can handle:
--
-- > ["hs", "lhs"]
--

known_extensions :: [FileExtension]
known_extensions = ["hs", "lhs"]


extractPrecis :: FilePath -> IO (Either CabalFileError CabalPrecis)
extractPrecis cabal_file =
    doesFileExist cabal_file >>= \exists -> 
    if exists then extractP cabal_file else cabalFileMissing cabal_file
  


cabalFileMissing :: FilePath -> IO (Either CabalFileError a)
cabalFileMissing file = return $ Left $ ERR_CABAL_FILE_MISSING file



extractP :: FilePath -> IO (Either CabalFileError CabalPrecis)
extractP cabal_file =
    liftM sk (safeReadGPD D.normal cabal_file)
  where
    sk = mapRight (extractCabalPrecis cabal_file)

safeReadGPD :: D.Verbosity -> FilePath 
            -> IO (Either CabalFileError D.GenericPackageDescription)
safeReadGPD verbo path = 
  catch (liftM Right $ D.readPackageDescription verbo path)
        (\e -> return $ Left $ ERR_CABAL_FILE_PARSE $ show e)


extractCabalPrecis :: FilePath -> D.GenericPackageDescription -> CabalPrecis
extractCabalPrecis path gpd =  
    CabalPrecis { package_name            = getName       gpd
                , package_version         = getVersion    gpd
                , path_to_cabal_file      = cabalFilePath path
                , cond_libraries          = getLibraries  gpd
                , cond_exes               = getExes       gpd
                }

--------------------------------------------------------------------------------
-- Extract from Package description

getName    :: D.GenericPackageDescription -> String
getName    = extractNameText    . D.package . D.packageDescription 

getVersion :: D.GenericPackageDescription -> String
getVersion = extractVersionText . D.package . D.packageDescription 


-- Getting \"Libraries\" is complicated due to Conditions...
--
-- Here all libraries are extracted, it seems typical that the 
-- Library within the PackageDescription is empty.
--
getLibraries :: D.GenericPackageDescription -> [CabalLibrary]
getLibraries gpd = filter (not . emptyLibrary) $ mbCons x xs
  where 
    x  = fmap extractLibrary $ D.library $ D.packageDescription $ gpd
    xs = fmap extractLibrary $ allLibraries $ gpd


emptyLibrary :: CabalLibrary -> Bool
emptyLibrary (CabalLibrary [] [] []) = True
emptyLibrary _                       = False

mbCons :: Maybe a -> [a] -> [a]
mbCons opt xs = maybe xs (:xs) $ opt 


-- Again, getting \"Executables\" is complicated by Conditions...
--
getExes :: D.GenericPackageDescription -> [CabalExe]
getExes gpd = xs ++ ys
   where 
     xs = map extractExe $ allExecutables gpd
     ys = map extractExe $ D.executables $ D.packageDescription gpd
                                  

extractNameText :: D.PackageIdentifier  -> String
extractNameText = fn . D.pkgName
  where fn (D.PackageName str) = str 

extractVersionText :: D.PackageIdentifier -> String
extractVersionText = fn . D.versionBranch . D.pkgVersion
  where fn = concat . intersperse "." . map show


extractLibrary :: D.Library -> CabalLibrary
extractLibrary lib  = 
    CabalLibrary { library_src_dirs = getSourceDirs  $ D.libBuildInfo lib
                 , public_modules   = mkExpos lib
                 , private_modules  = getPrivateModules $ D.libBuildInfo lib
                 }
  where
    mkExpos = map moduleDesc . D.exposedModules



extractExe :: D.Executable -> CabalExe
extractExe exe = 
    CabalExe { exe_main_module   = ExeMainPath       $ D.modulePath exe
             , exe_src_dirs      = getSourceDirs     $ D.buildInfo exe
             , exe_other_modules = getPrivateModules $ D.buildInfo exe
             }

getSourceDirs  :: D.BuildInfo -> [CabalSourceDir]
getSourceDirs  = map cabalSourceDir . D.hsSourceDirs    

getPrivateModules :: D.BuildInfo -> [ModuleDesc]
getPrivateModules = map moduleDesc . D.otherModules   


allLibraries :: D.GenericPackageDescription -> [D.Library]
allLibraries = maybe [] fn . D.condLibrary 
  where
   fn :: D.CondTree D.ConfVar [D.Dependency] D.Library -> [D.Library]
   fn = ctfold (:) []

 
allExecutables :: D.GenericPackageDescription -> [D.Executable]
allExecutables = concat . map (ctfold (:) [] . snd) . D.condExecutables



--------------------------------------------------------------------------------
-- General helper

ctfold :: (a -> b -> b) -> b -> (D.CondTree v c a) -> b
ctfold op initial node = foldr compfold x (D.condTreeComponents node)
  where
    x                           = D.condTreeData node `op` initial
    compfold (_,t1, Nothing) b  = ctfold op b t1
    compfold (_,t1, Just t2) b  = ctfold op (ctfold op b t1) t2
                         

