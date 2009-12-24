{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Scissoring
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 7 (Scissoring, Masking and Clearing) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Scissoring (
  -- * Tokens
  vg_MAX_SCISSOR_RECTS,

  vg_CLEAR_MASK,
  vg_FILL_MASK,
  vg_SET_MASK,
  vg_UNION_MASK,
  vg_INTERSECT_MASK,
  vg_SUBTRACT_MASK,

  -- * Functions
  vgMask,
  vgClear

) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101



