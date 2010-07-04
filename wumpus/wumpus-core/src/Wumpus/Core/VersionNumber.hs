{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.VersionNumber
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Version number
--
--------------------------------------------------------------------------------

module Wumpus.Core.VersionNumber
  ( 
    wumpus_core_version

  ) where


wumpus_core_version :: (Int,Int,Int)
wumpus_core_version = (0,21,0)
