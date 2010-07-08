{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.MicroPrint.VersionNumber
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

module Wumpus.MicroPrint.VersionNumber
  ( 
    wumpus_microprint_version

  ) where


wumpus_microprint_version :: (Int,Int,Int)
wumpus_microprint_version = (0,2,0)
