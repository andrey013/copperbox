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

import Data.Foldable ( foldrM )
import Data.List ( nub )
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
    liftM (getEIU . snd) $ runResolve (path_to_cabal_file precis) exts process
  where
    process     = do { mapM_ resolveLibrary (cond_libraries precis)
                     ; mapM_ resolveExe     (cond_exes      precis)
                     }
                
                

resolveLibrary :: CabalLibrary -> ResolveM ()
resolveLibrary lib@(CabalLibrary {library_src_dirs = src_dirs}) =
    do { mapM_ (resolveExposedModule src_dirs) (public_modules lib)
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
    liftIO (putStrLn $ show $ makeAll root exts) >>
    liftIO (putStrLn $ show (root,exts))         >>
    liftIO (putStrLn $ show src_dirs)            >>
    firstSuccess validFile (makeAll root exts)
  where
    makeAll root exts = 
        directoryProduct (\opt_dir ext -> fullPath root opt_dir mod_desc ext) 
                         src_dirs
                         exts
                            

directoryProduct :: (Maybe a -> b -> c) -> [a] -> [b] -> [c] 
directoryProduct f [] ys = [f Nothing  b | b <- ys]
directoryProduct f xs ys = [f (Just a) b | a <- xs , b <- ys ]

-- BAD ... this doesn't handle empty src_dirs...


crossProductWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossProductWith f xs ys = [f a b | a <- xs , b <- ys ]


fullPath :: CabalFilePath -> Maybe CabalSourceDir -> ModuleDesc -> FileExtension 
         -> FilePath
fullPath root opt_src_dir modu_desc ext = 
    FP.addExtension ext $ FP.joinPath $ concat parts
  where
    parts = case opt_src_dir of 
              Nothing      -> [ directoriesToCabalFile root
                              , moduleDirectories modu_desc ]
              Just src_dir -> [ directoriesToCabalFile root
                              , directoriesToSource    src_dir
                              , moduleDirectories      modu_desc ]





{-

publicModuleFiles   :: [CabalLibrary] 
                    -> ResolveM ([UnresolvedModule],[HsSourceFile])
publicModuleFiles   = extractAll library_src_dirs public_modules

privateModuleFiles  :: [CabalLibrary] 
                    -> ResolveM ([UnresolvedModule],[HsSourceFile])
privateModuleFiles  = extractAll library_src_dirs private_modules
  
exeModuleFiles      :: [CabalExe] 
                    -> ResolveM ([UnresolvedModule],[HsSourceFile])
exeModuleFiles      = extractAll exe_src_dirs exe_other_modules

extractAll :: (a -> [CabalSourceDir]) -> (a -> [ModuleDesc]) -> [a]
           -> ResolveM ([UnresolvedModule],[HsSourceFile])
extractAll srcF modF = liftM post . foldrM step (id,id) 
  where
    step a acc        = liftM (pconc acc) $ sourceFiles (srcF a) (modF a)

    pconc (f,g) (a,b) = (a `appendH` f, b `appendH` g)

    post (fs,gs)      = (nub $ toListH fs, nub $ toListH gs) 
  


sourceFiles :: [CabalSourceDir] -> [ModuleDesc] 
            -> ResolveM (H UnresolvedModule,H HsSourceFile)
sourceFiles src_dirs mods = foldrM fn (emptyH,emptyH) mods
  where
    fn md (us,ss) = resolveModuleLoc src_dirs md >>= \ans -> 
                    case ans of
                      Nothing   -> let m1 = UnresolvedModule $ moduleDescName md
                                   in return (m1 `consH` us,ss)
                      Just path -> let s1 = hsSourceFile (moduleDescName md) path
                                   in return (us, s1 `consH` ss)

-}


        
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
                                   getResolveM a  env st' >>= \(a,st'') -> 
                                   return (f a, st'')


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
validFile path = (liftIO $ putStrLn path) >> valid (liftIO . doesFileExist) path


logUnresolved :: ModName -> ResolveM ()
logUnresolved name = 
    sets_ $ star unresolveds 
                 (\us s -> s {unresolveds = UnresolvedModule name : us})
                   



-- The module might already be hidden or even exposed...
--
-- If already hidded  - don't add
-- If already exposed - don't add
-- 
logHidden :: ModName -> FilePath -> ResolveM ()
logHidden name path = sets_ $ star2 exposed_mods internal_mods upd
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
logExposed name path = sets_ $ star2 exposed_mods internal_mods upd
  where
    hs_src        = HsSourceFile name path
    upd exs ins s = if Set.member hs_src ins
                      then s { internal_mods = Set.delete hs_src ins
                             , exposed_mods  = optAdd hs_src exs }
                      else s { internal_mods = optAdd hs_src exs }

    optAdd a s = if Set.member a s then s else Set.insert a s
                     
