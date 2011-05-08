{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Slac.VersionNumber

-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Version number
--
--------------------------------------------------------------------------------

module Sound.Slac.VersionNumber
  ( 
    slac_version

  ) where

-- | Version number
--
-- > (0,1,0)
--
slac_version :: (Int,Int,Int)
slac_version = (0,3,0)
