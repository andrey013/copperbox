{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.PathUtils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.PathUtils
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

import Precis.ControlOperators
import Precis.Datatypes


import qualified Distribution.ModuleName        as D
import qualified Distribution.Text              as D

import Control.Monad
import Data.List ( intersperse )
import Data.Monoid
import System.Directory
import qualified System.FilePath                as FP


type PathRoot      = FilePath
type FileExtension = String

-- should have \".hs\" or \".lhs\" extension
exeModuleName :: FilePath -> Maybe D.ModuleName
exeModuleName = D.simpleParse . FP.dropExtension



buildFullPath :: PathRoot -> PathRoot -> D.ModuleName -> FileExtension 
              -> FilePath
buildFullPath root src_dir modu_name ext = 
    FP.addExtension ext $ FP.joinPath $ concatMap FP.splitPath parts
  where
    parts = [root, src_dir, D.toFilePath modu_name]


 

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


resolveFile :: PathRoot 
            -> [PathRoot] 
            -> D.ModuleName
            -> [FileExtension]
            -> IO (Maybe FilePath)
resolveFile path_root src_dirs modu_name exts =
   firstSuccess (valid doesFileExist . applyRoot) xpaths
  where
    xpaths                 :: [(PathRoot,FileExtension)]
    applyRoot              :: (PathRoot,FileExtension) -> FilePath

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