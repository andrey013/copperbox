{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.VersionNumber
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

module Wumpus.Basic.VersionNumber
  ( 
    wumpus_basic_version

  ) where


wumpus_basic_version :: (Int,Int,Int)
wumpus_basic_version = (0,1,0)
