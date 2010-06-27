{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Cabal.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatype for working with /Packages/ ...
--
--------------------------------------------------------------------------------


module Precis.Cabal.Datatypes
  (

    Package(..)

  , CabalFileError(..)
  , cabalFileErrorMsg

  , HsSourceFile(..)
  , hsSourceFile 
  , UnresolvedModule(..)


  ) where

import Precis.Cabal.InterimDatatypes

import qualified System.FilePath        as FP

-- Because of CondTree/Conditional a Cabal file can appear as
-- though it contains more than one Library, some normalization
-- has to be performed on this structure...
--
data Package = Package
      { package_name                :: String
      , package_version             :: String
      , exposed_modules             :: [HsSourceFile]
      , internal_modules            :: [HsSourceFile]
      , unresolved_modules          :: [UnresolvedModule]
      }
  deriving (Eq,Show)



data CabalFileError = ERR_CABAL_FILE_MISSING FilePath
                    | ERR_CABAL_FILE_PARSE   String
  deriving (Eq,Show)


cabalFileErrorMsg :: CabalFileError -> String
cabalFileErrorMsg (ERR_CABAL_FILE_MISSING s) = "*** Error: missing file - " ++ s
cabalFileErrorMsg (ERR_CABAL_FILE_PARSE   s) = "*** Error: parse error - " ++ s

-----

 
data HsSourceFile = HsSourceFile     
      { module_name            :: ModName
      , full_path_to           :: FilePath 
      }
  deriving (Eq,Ord,Show)

-- | An unresolved module couldn\'t be found in the listed source
-- directories.
--
newtype UnresolvedModule = UnresolvedModule { unresolved_name :: ModName }
  deriving (Eq,Ord,Show)


-- smart constructor

hsSourceFile :: ModName -> FilePath -> HsSourceFile
hsSourceFile name path = HsSourceFile name (FP.normalise path)

