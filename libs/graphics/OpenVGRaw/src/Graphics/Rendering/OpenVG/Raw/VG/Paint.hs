{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Paint
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 9 (Paint) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Paint (
  -- * Tokens
  vg_PAINT_TYPE, vg_PAINT_COLOR, vg_PAINT_COLOR_RAMP_SPREAD_MODE, 
  vg_PAINT_COLOR_RAMP_STOPS, vg_PAINT_COLOR_RAMP_PREMULTIPLIED, 
  vg_PAINT_LINEAR_GRADIENT, vg_PAINT_RADIAL_GRADIENT, 
  vg_PAINT_PATTERN_TILING_MODE,

  vg_PAINT_TYPE_COLOR, vg_PAINT_TYPE_LINEAR_GRADIENT, 
  vg_PAINT_TYPE_RADIAL_GRADIENT, vg_PAINT_TYPE_PATTERN,

  vg_COLOR_RAMP_SPREAD_PAD, vg_COLOR_RAMP_SPREAD_REPEAT, 
  vg_COLOR_RAMP_SPREAD_REFLECT,
            
  vg_TILE_FILL, vg_TILE_PAD, vg_TILE_REPEAT, vg_TILE_REFLECT,

  -- * Functions
  vgCreatePaint, 
  vgDestroyPaint, 
  vgSetPaint, 
-- Missing in ShivaVG?
--  vgGetPaint,  
--  vgSetColor,
--  vgGetColor,
  vgPaintPattern
  
) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101

