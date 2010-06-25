{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Cabal.PathUtils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.Cabal.PathUtils
  (
    FileExtension

  , ResolveM
  , runResolve
  , resolveModuleLoc

  , publicModuleFiles
  , privateModuleFiles
  , exeModuleFiles

  ) where

import Precis.Cabal.Datatypes
import Precis.Utils.ControlOperators

import Control.Applicative
import Control.Monad
import Data.Foldable ( foldrM )
import System.Directory
import qualified System.FilePath                as FP


-- Are there any more useful manipulations than simply 
-- resolving module_names to file_paths?

type FileExtension = String


resolveModuleLoc :: [CabalSourceDir] -> ModuleDesc -> ResolveM (Maybe (FilePath))
resolveModuleLoc src_dirs mod_desc = 
    asks root_path  >>= \root -> 
    asks known_exts >>= \exts ->
    firstSuccess validFile (makeAll root exts)
  where
    makeAll root exts = 
        crossProductWith (\src_dir ext -> fullPath root src_dir mod_desc ext) 
                         src_dirs
                         exts
                            



crossProductWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossProductWith f xs ys = [f a b | a <- xs , b <- ys ]

fullPath :: CabalFilePath -> CabalSourceDir -> ModuleDesc -> FileExtension 
         -> FilePath
fullPath root src_dir modu_desc ext = 
    FP.addExtension ext $ FP.joinPath $ concat parts
  where
    parts = [ directoriesToCabalFile root 
            , directoriesToSource    src_dir
            , moduleDirectories       modu_desc
            ]

publicModuleFiles :: CabalLibrary -> ResolveM ([UnresolvedModule],[SourceFile])
publicModuleFiles lib = 
    sourceFiles (library_src_dirs lib) (public_modules lib)
  
privateModuleFiles :: CabalLibrary -> ResolveM ([UnresolvedModule],[SourceFile])
privateModuleFiles lib = 
    sourceFiles (library_src_dirs lib) (private_modules lib)
  
exeModuleFiles :: CabalExe -> ResolveM ([UnresolvedModule],[SourceFile])
exeModuleFiles exe = 
    sourceFiles (exe_src_dirs exe) (exe_other_modules exe) 


sourceFiles :: [CabalSourceDir] -> [ModuleDesc] 
            -> ResolveM ([UnresolvedModule],[SourceFile])
sourceFiles src_dirs mods = foldrM fn ([],[]) mods
  where
    fn md (us,ss) = resolveModuleLoc src_dirs md >>= \ans -> 
                    case ans of
                      Nothing   -> let m1 = UnresolvedModule $ moduleDescName md
                                   in return (m1:us,ss)
                      Just path -> let s1 = sourceFile (moduleDescName md) path
                                   in return (us, s1:ss)


--------------------------------------------------------------------------------
--

data REnv = REnv { root_path :: CabalFilePath, known_exts :: [FileExtension] }

newtype ResolveM a = ResolveM { getResolveM :: REnv -> IO a }

instance Functor ResolveM where
  fmap f mf = ResolveM $ \env -> getResolveM mf env >>= \a -> return (f a)

instance Applicative ResolveM where
  pure a   = ResolveM $ \_   -> return a
  af <*> a = ResolveM $ \env -> getResolveM af env >>= \f ->
                                liftM f (getResolveM a  env)


instance Monad ResolveM where
  return a = ResolveM $ \_   -> return a
  m >>= k  = ResolveM $ \env -> getResolveM m env >>= \a -> 
                                getResolveM (k a) env


runResolve :: CabalFilePath -> [FileExtension] -> ResolveM a -> IO a
runResolve root exts mf = getResolveM mf $ env
  where
    env = REnv { root_path = root, known_exts = exts }

ask :: ResolveM REnv
ask = ResolveM $ \env -> return env

asks :: (REnv -> a) -> ResolveM a
asks f = liftM f ask

liftIO :: IO a -> ResolveM a
liftIO ma = ResolveM $ \_ -> ma

validFile :: FilePath -> ResolveM (Maybe FilePath)
validFile path = valid (liftIO . doesFileExist) path



