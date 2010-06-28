{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Cabal
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Import module for Cabal libs
--
--------------------------------------------------------------------------------


module Precis.Cabal
  (
    module Precis.Cabal.CabalPackage 
  , module Precis.Cabal.Datatypes

  , ModName
  , getModName

  ) where

import Precis.Cabal.CabalPackage 
import Precis.Cabal.Datatypes
import Precis.Cabal.InterimDatatypes ( ModName, getModName ) 

{-

-- | Extract all the exposed modules in a cabal file.
--
exposedModules :: CabalPrecis -> IO ([UnresolvedModule],[HsSourceFile])
exposedModules precis = 
    runResolve path known_extensions (publicModuleFiles libs)
  where
    path = path_to_cabal_file precis
    libs = cond_libraries precis

-- TODO - we can't get the /private/ modules without getting 
-- the /public/ ones and checking seemingly private modules
-- aren't exported elsewhere as public modules (e.g. an 
-- exe definition in a cabal file can list modules that are 
-- also publicly in a library definition).
-- 
-- As it stands, a client using the CabalPrecis datatype 
-- has to do an awful lot of post-processing to get at a 
-- module list, so this is strongly indicating that the 
-- CabalPrecis data type isn't adequate. Although its 
-- construction didn't handle conditions well, the original 
-- version 0.4.0 Precis data now looks a better bet than the 
-- new one
--


data AllModules = AllModules 
      { exposed_modules  :: ([UnresolvedModule],[HsSourceFile])
      , internal_modules :: ([UnresolvedModule],[HsSourceFile])
      }

allModules :: CabalPrecis -> IO AllModules
allModules precis = undefined
  where
    path   = path_to_cabal_file precis
    libs   = cond_libraries     precis
    exes   = cond_exes          precis
    lprivs = runResolve path known_extensions (privateModuleFiles libs)
    eprivs = runResolve path known_extensions (exeModuleFiles exes)

-}