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
    exeModuleName
  , resolveFiles
  , removePrefix
  , resolveToCabalFileLoc
  ) where

import Precis.Datatypes

import qualified Distribution.ModuleName        as D

import Data.List ( intersperse )
import Data.Monoid
import System.Directory
import qualified System.FilePath                as FP


-- should have \".hs\" or \".lhs\" extension
exeModuleName :: FilePath -> D.ModuleName
exeModuleName = D.fromString . FP.dropExtension

resolveFiles :: FilePath 
             -> [FilePath] 
             -> [D.ModuleName] 
             -> [String]
             -> IO [SourceFile]
resolveFiles path_root src_dirs mod_names exts = 
    let cp_paths = map fn $ longCrossProduct src_dirs mod_names in
    mapM resolve cp_paths 
  where
    fn (path,modu) = (mname modu, moduleLongPath path_root path modu)

    resolve (mod_name,path) = do { ans <- findByExtension path exts
                                 ; case ans of
                                     Nothing ->         
                                         return $ UnresolvedFile $ mod_name
                                     Just path' -> 
                                         return $ sourceFile mod_name path'
                                 }


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