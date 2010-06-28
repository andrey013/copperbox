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
    module Precis.Cabal.Datatypes
  , ModName
  , getModName

  , extractPackageInfo

  ) where

import Precis.Cabal.CabalPackage 
import Precis.Cabal.Datatypes
import Precis.Cabal.InterimDatatypes
import Precis.Cabal.ResolveM

import Control.Monad
 
extractPackageInfo :: FilePath -> IO (Either CabalFileError Package)
extractPackageInfo path = 
    extractPrecis path >>= \ans -> 
    case ans of 
      Left err -> return $ Left err
      Right precis -> liftM Right $ buildPackage precis

buildPackage :: CabalPrecis -> IO Package
buildPackage precis = resolvePrecis precis known_extensions >>= \(e,i,u) -> 
   return $ Package { package_name       = pkg_name $ precis
                    , package_version    = pkg_version $ precis
                    , exposed_modules    = e
                    , internal_modules   = i
                    , unresolved_modules = u
                    }
          

