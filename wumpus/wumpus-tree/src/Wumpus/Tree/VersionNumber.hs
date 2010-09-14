{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree.VersionNumber
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

module Wumpus.Tree.VersionNumber
  ( 
    wumpus_tree_version

  ) where

-- | Version number
--
-- > (0,6,0)
--
wumpus_tree_version :: (Int,Int,Int)
wumpus_tree_version = (0,6,0)
