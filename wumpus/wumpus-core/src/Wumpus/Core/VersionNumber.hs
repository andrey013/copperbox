{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.VersionNumber
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Version number.
--
--------------------------------------------------------------------------------

module Wumpus.Core.VersionNumber
  ( 
    wumpus_core_version

  ) where

-- | Version number.
--
-- > (0,45,0)
--
wumpus_core_version :: (Int,Int,Int)
wumpus_core_version = (0,45,0)
