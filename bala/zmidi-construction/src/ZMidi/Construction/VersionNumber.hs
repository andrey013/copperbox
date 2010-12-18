{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Construction.VersionNumber
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

module ZMidi.Construction.VersionNumber
  ( 
    zmidi_construction_version

  ) where

-- | Version number
--
-- > (0,1,0)
--
zmidi_construction_version :: (Int,Int,Int)
zmidi_construction_version = (0,1,0)
