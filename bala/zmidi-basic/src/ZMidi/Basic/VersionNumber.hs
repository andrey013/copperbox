{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.VersionNumber
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

module ZMidi.Basic.VersionNumber
  ( 
    zmidi_basic_version

  ) where

-- | Version number
--
-- > (0,1,0)
--
zmidi_basic_version :: (Int,Int,Int)
zmidi_basic_version = (0,1,0)
