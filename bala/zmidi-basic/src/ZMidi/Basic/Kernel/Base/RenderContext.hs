{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Base.RenderContext
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Base.RenderContext
  ( 

    RenderContext(..)

  ) where

import Data.Word

data RenderContext = RenderContext 
      { rc_volume               :: !Word16
      , rc_pan                  :: !Word16 
      , rc_noteon_velo          :: !Word8
      , rc_note_off_velo        :: !Word8
      } 