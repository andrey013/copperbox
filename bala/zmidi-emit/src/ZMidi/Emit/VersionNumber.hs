{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.VersionNumber
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

module ZMidi.Emit.VersionNumber
  ( 
    zmidi_emit_version

  ) where

-- | Version number
--
-- > (0,1,0)
--
zmidi_emit_version :: (Int,Int,Int)
zmidi_emit_version = (0,1,0)
