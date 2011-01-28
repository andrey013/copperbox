{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Block.VersionNumber
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Version number.
--
--------------------------------------------------------------------------------

module Wumpus.Block.VersionNumber
  ( 
    wumpus_block_version

  ) where

-- | Version number
--
-- > (0,1,0)
--
wumpus_block_version :: (Int,Int,Int)
wumpus_block_version = (0,1,0)
