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
   
  -- * Type synonyms
    PathRoot
  , FileExtension

  -- * Operations
  , buildFullPath
  , exeModuleName
  , resolveFiles
  , removePrefix
  , resolveToCabalFileLoc
  ) where

import Precis.Cabal.Datatypes
import Precis.Utils.ControlOperators


import qualified Distribution.ModuleName        as D
import qualified Distribution.Text              as D

import Control.Applicative
import Control.Monad
import Data.List ( intersperse )
import Data.Monoid
import System.Directory
import qualified System.FilePath                as FP


type FileExtension = String

-- should have \".hs\" or \".lhs\" extension
exeModuleName :: FilePath -> Maybe D.ModuleName
exeModuleName = D.simpleParse . FP.dropExtension



buildFullPath :: CabalFilePath -> CabalSourceDir -> D.ModuleName -> FileExtension 
              -> FilePath
buildFullPath root src_dir modu_name ext = 
    FP.addExtension ext $ FP.joinPath $ concat parts
  where
    parts = [ directoriesToCabalFile root 
            , directoriesToSource src_dir
            , FP.splitPath $ D.toFilePath modu_name
            ]

type PathRoot = FilePath -- OLD
 

resolveFiles :: PathRoot
             -> [PathRoot] 
             -> [D.ModuleName] 
             -> [FileExtension]
             -> IO [SourceFile]
resolveFiles path_root src_dirs mod_names exts = undefined
--    let cp_paths = map fn $ longCrossProduct src_dirs mod_names in
--    mapM resolve cp_paths 
  where
    fn (path,modu) = (mname modu, moduleLongPath path_root path modu)

    resolve (mod_name,path) = do { ans <- findByExtension path exts
                                 ; case ans of
                                     Nothing ->         
                                         return $ undefined -- UnresolvedFile $ mod_name
                                     Just path' -> 
                                         return $ sourceFile mod_name path'
                                 }


resolveFile :: CabalFilePath 
            -> [CabalSourceDir] 
            -> D.ModuleName
            -> [FileExtension]
            -> IO (Maybe FilePath)
resolveFile path_root src_dirs modu_name exts =
   firstSuccess (valid doesFileExist . applyRoot) xpaths
  where
    xpaths                 :: [(CabalSourceDir,FileExtension)]
    applyRoot              :: (CabalSourceDir,FileExtension) -> FilePath

    xpaths                 = crossProduct src_dirs exts
    applyRoot (to_src,ext) = buildFullPath path_root to_src modu_name ext



findByExtension :: FilePath -> [String] -> IO (Maybe FilePath)
findByExtension _    []     = return Nothing
findByExtension path (e:es) = let full = FP.addExtension path e in 
    doesFileExist full >>= \ans -> if ans then return (Just full) 
                                          else findByExtension path es


moduleLongPath :: FilePath -> FilePath -> D.ModuleName -> FilePath
moduleLongPath root src_dir mod_name = 
    FP.joinPath $ FP.splitPath root ++ FP.splitPath src_dir ++ D.components mod_name 


longCrossProduct :: Monoid a => [a] -> [b] -> [(a,b)]
longCrossProduct [] ys = map (\b -> (mempty,b)) ys
longCrossProduct xs ys = [(a,b) | a <- xs , b <- ys ]

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct xs ys = [(a,b) | a <- xs , b <- ys ]

mname :: D.ModuleName -> String 
mname = concat . intersperse "." . D.components

--------------------------------------------------------------------------------

removePrefix :: FilePath -> FilePath -> FilePath
removePrefix pre path = FP.joinPath $ step (fn pre) (fn path) 
  where
    fn                          = FP.splitPath . FP.normalise
    step (x:xs) (y:ys) | x == y = step xs ys 
    step _      ys              = ys

--------------------------------------------------------------------------------

resolveToCabalFileLoc :: FilePath -> FilePath -> FilePath
resolveToCabalFileLoc cabal_file src_file = 
    (FP.dropFileName cabal_file) `FP.combine` src_file 



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

resolveModule :: CabalSourceDir -> D.ModuleName 
                    -> ResolveM (Either UnresolvedModule SourceFile)
resolveModule rel_src_dir modu_name = 
    asks root_path  >>= \root -> 
    asks known_exts >>= \exts ->
    firstSuccess (validFile . resPath root) exts >>= \ans ->
    case ans of 
      Nothing -> return $ Left (UnresolvedModule modu_name)
      Just path -> return $ Right (srcFile path)

  where
    resPath root = \ext -> buildFullPath root rel_src_dir modu_name ext
    srcFile full_path = sourceFile modu_name full_path