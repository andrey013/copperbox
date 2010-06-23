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
   
    extractPrecis
  , known_extensions

  ) where

import Precis.ControlOperators
import Precis.Datatypes
import Precis.PathUtils
import Precis.Utils

import qualified Distribution.ModuleName                as D
import qualified Distribution.Package                   as D
import qualified Distribution.PackageDescription        as D
import qualified Distribution.PackageDescription.Parse  as D
import qualified Distribution.Verbosity                 as D
import qualified Distribution.Version                   as D

import Control.Applicative
import Control.Monad
import Data.List ( intersperse, nub )
import System.Directory
import System.FilePath


data ExePrecis = ExePrecis 
      { exe_module_path     :: FilePath
      , exe_other_modules   :: [D.ModuleName]
      , exe_source_dirs     :: [FilePath]
      }


extractPrecis :: FilePath -> [FileExtension] -> IO (Either CabalFileError CabalPrecis)
extractPrecis cabal_file known_exts = do
    exists   <- doesFileExist cabal_file
    if exists then sk 
              else return $ Left $ ERR_CABAL_FILE_MISSING cabal_file
  where
    sk = liftM (mapRight nubSourceFiles) $ extractP cabal_file known_exts

-- | File extensions that Precis can handle:
--
-- > ["hs", "lhs"]
--
known_extensions :: [FileExtension]
known_extensions = ["hs", "lhs"]




extractP :: FilePath -> [FileExtension] -> IO (Either CabalFileError CabalPrecis)
extractP cabal_file_path exts =
    safeReadPackageDescription D.normal cabal_file_path `onSuccessM` sk
  where
    root_to_cabal = dropFileName cabal_file_path  
    sk gen_pkg = do { (expos,privs) <- getSourceFiles gen_pkg root_to_cabal exts
                    ; return $ CabalPrecis 
                                 { package_name          = getName gen_pkg
                                 , package_version       = getVersion gen_pkg
                                 , path_to_cabal_file    = cabal_file_path
                                 , exposed_modules       = expos
                                 , internal_modules      = privs
                                 , unresolved_modules    = []
                                 }
                    }

type SafeGPD = Either CabalFileError D.GenericPackageDescription

safeReadPackageDescription :: D.Verbosity -> FilePath -> IO SafeGPD
safeReadPackageDescription verbo path = 
  catch (liftM Right $ D.readPackageDescription verbo path)
        (\e -> return $ Left $ ERR_CABAL_FILE_PARSE $ show e)

--------------------------------------------------------------------------------
-- Extract from Package description

getName    :: D.GenericPackageDescription -> String
getName    = extrNameText    . D.package . D.packageDescription 

getVersion :: D.GenericPackageDescription -> String
getVersion = extrVersionText . D.package . D.packageDescription 
                                  

extrNameText :: D.PackageIdentifier  -> String
extrNameText = fn . D.pkgName
  where fn (D.PackageName str) = str 

extrVersionText :: D.PackageIdentifier -> String
extrVersionText = fn . D.versionBranch . D.pkgVersion
  where fn = concat . intersperse "." . map show



-- extract source files

getSourceFiles :: D.GenericPackageDescription 
               -> FilePath
               -> [FileExtension] 
               -> IO ([SourceFile], [SourceFile])
getSourceFiles pkg_desc root exts = do 
    lib_mods <- mapM (resolveLibrary root exts)    $ allLibraries   pkg_desc
    exe_mods <- mapM (resolveExecutable root exts) $ allExecutables pkg_desc
    let (lib_expos, lib_privs) = foldr fn ([],[]) lib_mods 
    return (lib_expos, lib_privs ++ concat exe_mods)
  where 
    fn (a,b) (xs,ys) = (a++xs,b++ys)                                 


allLibraries :: D.GenericPackageDescription -> [D.Library]
allLibraries = maybe [] fn . D.condLibrary 
  where
   fn :: D.CondTree D.ConfVar [D.Dependency] D.Library -> [D.Library]
   fn = ctfold (:) []

 
allExecutables :: D.GenericPackageDescription -> [D.Executable]
allExecutables = concat . map (ctfold (:) [] . snd) . D.condExecutables


resolveLibrary :: FilePath 
               -> [String] 
               -> D.Library 
               -> IO ([SourceFile], [SourceFile])
resolveLibrary root exts lib = liftM2 (,) (fn expos) (fn others)
  where
    fn mods                    = resolveFiles root src_paths mods exts
    (src_paths, expos, others) = libraryContents lib  

libraryContents :: D.Library -> ([FilePath], [D.ModuleName], [D.ModuleName])
libraryContents lib = (src_paths, expo_modules, other_modules)
  where
    src_paths       = D.hsSourceDirs   $ D.libBuildInfo lib
    expo_modules    = D.exposedModules lib
    other_modules   = D.otherModules   $ D.libBuildInfo lib

resolveExecutable :: FilePath -> [String] -> D.Executable -> IO [SourceFile]
resolveExecutable root exts exe = resolveFiles root src_paths mods exts
  where
    (src_paths, mods) = ([],[]) -- executableModules exe


-- All executable modules considered internal...
{-
executableModules :: D.Executable -> ([FilePath], [D.ModuleName])
executableModules = fn . executableContents 
  where
    fn (as,exe,cs) = (as, maybe cs (:cs) $ exeModuleName exe)
    --
    -- WARNING - this is bad ignoring an error...
-}

-- exe_module_path likely to have an extension but the 
-- path will be relative to source_dirs

executableContents :: D.Executable -> ExePrecis
executableContents exe = ExePrecis
      { exe_module_path     = D.modulePath exe
      , exe_other_modules   = D.otherModules   $ D.buildInfo exe
      , exe_source_dirs     = D.hsSourceDirs   $ D.buildInfo exe
      }


--------------------------------------------------------------------------------
-- reconcile modules

-- Initially a precis may have dublicates (via Cabal\'s 
-- conditional mechanism).
--
nubSourceFiles :: CabalPrecis -> CabalPrecis
nubSourceFiles  = 
    pstar2 (\xs ys s  -> let (xs',ys') = differentiate xs ys  
              in s { exposed_modules = xs', internal_modules = ys' })
           exposed_modules internal_modules
  where
    differentiate xs ys = (xs', ys') where 
       xs' = nub xs
       ys' = filter (\x -> not (x `elem` xs')) $ nub ys
 

--------------------------------------------------------------------------------
-- General helper

ctfold :: (a -> b -> b) -> b -> (D.CondTree v c a) -> b
ctfold op initial node = foldr compfold x (D.condTreeComponents node)
  where
    x                           = D.condTreeData node `op` initial
    compfold (_,t1, Nothing) b  = ctfold op b t1
    compfold (_,t1, Just t2) b  = ctfold op (ctfold op b t1) t2
                         

--------------------------------------------------------------------------------
--

data REnv = REnv { root_path :: FilePath, known_exts :: [FileExtension] }

newtype ResolverM a = ResolverM { getResolverM :: REnv -> IO a }

instance Functor ResolverM where
  fmap f mf = ResolverM $ \env -> getResolverM mf env >>= \a -> return (f a)

instance Applicative ResolverM where
  pure a   = ResolverM $ \_   -> return a
  af <*> a = ResolverM $ \env -> getResolverM af env >>= \f ->
                                 liftM f (getResolverM a  env)


instance Monad ResolverM where
  return a = ResolverM $ \_   -> return a
  m >>= k  = ResolverM $ \env -> getResolverM m env >>= \a -> 
                                 getResolverM (k a) env


ask :: ResolverM REnv
ask = ResolverM $ \env -> return env

asks :: (REnv -> a) -> ResolverM a
asks f = liftM f ask

liftIO :: IO a -> ResolverM a
liftIO ma = ResolverM $ \_ -> ma

validFile :: FilePath -> ResolverM (Maybe FilePath)
validFile path = valid (liftIO . doesFileExist) path

resolve :: FilePath -> D.ModuleName 
                    -> ResolverM (Either UnresolvedModule SourceFile)
resolve rel_src_dir modu_name = 
    asks root_path  >>= \root -> 
    asks known_exts >>= \exts ->
    firstSuccess (validFile . resPath root) exts >>= \ans ->
    case ans of 
      Nothing -> return $ Left (UnresolvedModule modu_name)
      Just path -> return $ Right (srcFile path)

  where
    resPath root = \ext -> buildFullPath root rel_src_dir modu_name ext
    srcFile full_path = SourceFile modu_name full_path