{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Cabal.ResolveM
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A Monad transofrmer (over IO) for revolving module names 
-- to FilePaths.
--
--------------------------------------------------------------------------------


module Precis.Cabal.ResolveM
  (

    resolvePrecis
  , ResolveM
  , runResolve
  , getFilePathLoc
  , getEIU

  ) where

import Precis.Cabal.Datatypes
import Precis.Cabal.InterimDatatypes
import Precis.Utils.Common
import Precis.Utils.ControlOperators

import Control.Applicative
import Control.Monad

import Data.Set ( Set )
import qualified Data.Set                       as Set
import System.Directory
import qualified System.FilePath                as FP



-- Are there any more useful manipulations than simply 
-- resolving module_names to file_paths?

resolvePrecis :: CabalPrecis 
              -> [FileExtension]
              -> IO ([HsSourceFile],[HsSourceFile],[UnresolvedModule])
resolvePrecis precis exts =
    do { (_,st) <- runResolve (path_to_cabal_file precis) exts process
       ; return $ getEIU st
       }
  where
    process     = do { mapM_ resolveLibrary (cond_libraries precis)
                     ; mapM_ resolveExe     (cond_exes      precis)
                     }
                
                

resolveLibrary :: CabalLibrary -> ResolveM ()
resolveLibrary lib@(CabalLibrary {library_src_dirs = src_dirs}) =
    do { mapM_ (resolveExposedModule src_dirs) (public_modules  lib)
       ; mapM_ (resolveHiddenModule  src_dirs) (private_modules lib)
       }

    

resolveExe :: CabalExe -> ResolveM ()
resolveExe exe@(CabalExe {exe_src_dirs=src_dirs}) = 
    do { mapM_ (resolveHiddenModule  src_dirs) (exe_other_modules exe)
       ; return ()  -- THE EXE MODULE TODO...
       }


resolveExposedModule :: [CabalSourceDir] -> ModuleDesc -> ResolveM ()
resolveExposedModule = resolveModule logExposed

resolveHiddenModule :: [CabalSourceDir] -> ModuleDesc -> ResolveM ()
resolveHiddenModule = resolveModule logHidden

resolveModule :: (ModName -> FilePath -> ResolveM ()) 
              -> [CabalSourceDir] -> ModuleDesc -> ResolveM ()
resolveModule sk src_dirs mod_desc =
    getFilePathLoc src_dirs mod_desc >>= \ans ->
    case ans of 
      Nothing -> logUnresolved (moduleDescName mod_desc) 
      Just path -> sk (moduleDescName mod_desc) path


getFilePathLoc :: [CabalSourceDir] -> ModuleDesc -> ResolveM (Maybe (FilePath))
getFilePathLoc src_dirs mod_desc = 
    asks root_path  >>= \root -> 
    asks known_exts >>= \exts ->
    firstSuccess validFile (makeAll root exts)
  where
    makeAll root exts = 
        directoryProduct (\opt_dir ext -> fullPath root opt_dir mod_desc ext) 
                         src_dirs
                         exts
                            

directoryProduct :: (Maybe a -> b -> c) -> [a] -> [b] -> [c] 
directoryProduct f [] ys = [f Nothing  b | b <- ys]
directoryProduct f xs ys = [f (Just a) b | a <- xs , b <- ys ]


fullPath :: CabalFilePath -> Maybe CabalSourceDir -> ModuleDesc -> FileExtension 
         -> FilePath
fullPath root opt_src_dir mdesc ext = FP.normalise $ 
    prepend (directoriesToCabalFile root) $ 
    prepend (maybe [] directoriesToSource opt_src_dir) $ 
    modulePath mdesc ext         

prepend :: [FilePath] -> FilePath -> FilePath
prepend xs = FP.combine (FP.joinPath xs)

modulePath :: ModuleDesc -> FileExtension -> FilePath
modulePath mdesc ext = step (moduleDirectories mdesc) 
  where
    step []     = ext      -- Really an error?
    step [a]    = FP.addExtension a ext
    step (a:as) = FP.combine a $ step as





        
--------------------------------------------------------------------------------
--


data RSt = RSt { internal_mods  :: Set HsSourceFile
               , exposed_mods   :: Set HsSourceFile
               , unresolveds    :: [UnresolvedModule]
               }

stateZero :: RSt 
stateZero = RSt Set.empty Set.empty []

getEIU :: RSt -> ([HsSourceFile],[HsSourceFile],[UnresolvedModule])
getEIU rst = ( Set.toList $ exposed_mods rst
             , Set.toList $ internal_mods rst 
             , unresolveds rst )


data REnv = REnv { root_path :: CabalFilePath, known_exts :: [FileExtension] }

newtype ResolveM a = ResolveM { getResolveM :: REnv -> RSt -> IO (a,RSt) }

instance Functor ResolveM where
  fmap f mf = ResolveM $ \env st -> getResolveM mf env st >>= \(a,st') -> 
                                    return (f a,st')


instance Applicative ResolveM where
  pure a   = ResolveM $ \_   st -> return (a,st)
  af <*> a = ResolveM $ \env st -> getResolveM af env st  >>= \(f,st')  ->
                                   getResolveM a  env st' >>= \(b,st'') -> 
                                   return (f b, st'')


instance Monad ResolveM where
  return a = ResolveM $ \_   st -> return (a,st)
  m >>= k  = ResolveM $ \env st -> getResolveM m env st >>= \(a,st') -> 
                                   getResolveM (k a) env st' 
                                   


runResolve :: CabalFilePath -> [FileExtension] -> ResolveM a -> IO (a,RSt)
runResolve root exts mf = getResolveM mf env stateZero
  where
    env = REnv { root_path = root, known_exts = exts }

ask :: ResolveM REnv
ask = ResolveM $ \env st -> return (env,st)

asks :: (REnv -> a) -> ResolveM a
asks f = liftM f ask

get :: ResolveM RSt
get = ResolveM $ \_ st -> return (st,st)

set :: RSt -> ResolveM ()
set st = ResolveM $ \_ _ -> return ((),st)

sets :: (RSt -> (a,RSt)) -> ResolveM a
sets f = ResolveM $ \_ st -> return (f st)

sets_ :: (RSt -> RSt) -> ResolveM ()
sets_ f = ResolveM $ \_ st -> return ((), f st)


liftIO :: IO a -> ResolveM a
liftIO ma = ResolveM $ \_ st -> ma >>= \a -> return (a,st)

validFile :: FilePath -> ResolveM (Maybe FilePath)
validFile path = valid (liftIO . doesFileExist) path


logUnresolved :: ModName -> ResolveM ()
logUnresolved name = sets_ (star unresolveds upd)
   where
     upd us s = s {unresolveds = UnresolvedModule name : us}
                   



-- The module might already be hidden or even exposed...
--
-- If already hidded  - don't add
-- If already exposed - don't add
-- 
logHidden :: ModName -> FilePath -> ResolveM ()
logHidden name path = sets_ (star2 exposed_mods internal_mods upd)
  where
    hs_src        = HsSourceFile name path
    upd exs ins s = if Set.member hs_src exs 
                      then s
                      else s { internal_mods = Set.insert hs_src ins }
                     

-- The module might already be exposed or hidden...
--
-- If already hidded  - remove from hidden list add to exposed list
-- If already exposed - don't add
-- 
logExposed :: ModName -> FilePath -> ResolveM ()
logExposed name path = sets_ (star2 exposed_mods internal_mods upd)
  where
    hs_src        = HsSourceFile name path
    upd exs ins s = if Set.member hs_src ins
                      then s { internal_mods = Set.delete hs_src ins
                             , exposed_mods  = optAdd hs_src exs }
                      else s { exposed_mods  = optAdd hs_src exs }

    optAdd a s = if Set.member a s then s else Set.insert a s
                     
