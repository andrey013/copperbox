{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.VersionNumber
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

module Wumpus.Extra.VersionNumber
  ( 
    wumpus_extra_version

  ) where


wumpus_extra_version :: (Int,Int,Int)
wumpus_extra_version = (0,12,0)
