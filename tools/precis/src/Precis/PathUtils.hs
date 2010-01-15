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
    moduleLongPath
  , moduleFind

  ) where

import Precis.Utils

import System.Directory
import System.FilePath

moduleLongPath :: FilePath -> FilePath -> FilePath
moduleLongPath src_dir mod_name = 
    joinPath $ splitPath src_dir ++ splitPath mod_name 


moduleFind :: FilePath -> String -> IO (Maybe FilePath)
moduleFind path ext = predMaybeM doesFileExist (path `addExtension` ext)