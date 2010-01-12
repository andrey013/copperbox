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
  , CabalField(..)

  ) where


data CabalInfo = CabalInfo {
        cab_package_name        :: String,
        cab_version_number      :: String,
        cab_decription          :: Maybe String
      }
  deriving (Eq,Show)
      

data CabalField = CabalField { field_name :: String, field_data :: String }
  deriving (Eq,Show)