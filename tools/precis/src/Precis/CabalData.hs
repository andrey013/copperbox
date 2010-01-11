{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.CabalData
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Data types for fragment of Cabal files that Precis uses.
--
--------------------------------------------------------------------------------


module Precis.CabalData
  (
    CabalInfo(..)

  ) where


data CabalInfo = CabalInfo {
        cab_package_name        :: String,
        cab_version_number      :: String,
        cab_decription          :: String
      }
  deriving (Eq,Show)
      

