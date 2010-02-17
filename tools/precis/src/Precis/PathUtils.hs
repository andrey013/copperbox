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
  , resolveModules

  ) where

import Precis.Datatypes

import Distribution.ModuleName

import Data.List ( intersperse )
import Data.Monoid
import System.Directory
import System.FilePath


-- should have \".hs\" or \".lhs\" extension
exeModuleName :: FilePath -> ModuleName
exeModuleName = fromString . dropExtension

resolveModules :: FilePath 
               -> [FilePath] 
               -> [ModuleName] 
               -> [String]
               -> IO [SourceModule]
resolveModules path_root src_dirs mod_names exts = 
    let cp_paths = map fn $ longCrossProduct src_dirs mod_names in
    mapM resolve cp_paths 
  where
    fn (path,modu) = (mname modu, moduleLongPath path_root path modu)

    resolve (mod_name,path) = do { ans <- findByExtension path exts
                                 ; case ans of
                                     Nothing ->         
                                         return $ UnresolvedModule $ mod_name
                                     Just path' -> 
                                         return $ sourceModule mod_name path'
                                 }
findByExtension :: FilePath -> [String] -> IO (Maybe FilePath)
findByExtension _    []     = return Nothing
findByExtension path (e:es) = let full = addExtension path e in 
    doesFileExist full >>= \ans -> if ans then return (Just full) 
                                          else findByExtension path es


moduleLongPath :: FilePath -> FilePath -> ModuleName -> FilePath
moduleLongPath root src_dir mod_name = 
    joinPath $ splitPath root ++ splitPath src_dir ++ components mod_name 


longCrossProduct :: Monoid a => [a] -> [b] -> [(a,b)]
longCrossProduct [] ys = map (\b -> (mempty,b)) ys
longCrossProduct xs ys = [(a,b) | a <- xs , b <- ys ]


mname :: ModuleName -> String 
mname = concat . intersperse "." . components