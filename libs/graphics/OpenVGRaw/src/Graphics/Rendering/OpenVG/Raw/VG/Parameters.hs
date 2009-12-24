{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Parameters
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 5 (Setting API Parameters) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Parameters (
  -- * Tokens
  vg_MATRIX_MODE, 
  vg_FILL_RULE, 
  vg_IMAGE_QUALITY, 
  vg_RENDERING_QUALITY, 
  vg_BLEND_MODE, 
  vg_IMAGE_MODE, 

  vg_SCISSOR_RECTS, 

  vg_STROKE_LINE_WIDTH, 
  vg_STROKE_CAP_STYLE, 
  vg_STROKE_JOIN_STYLE, 
  vg_STROKE_MITER_LIMIT, 
  vg_STROKE_DASH_PATTERN, 
  vg_STROKE_DASH_PHASE, 
  vg_STROKE_DASH_PHASE_RESET,

  vg_TILE_FILL_COLOR, 

  vg_CLEAR_COLOR, 

  vg_MASKING,
  vg_SCISSORING, 

  vg_PIXEL_LAYOUT, 
  vg_SCREEN_LAYOUT, 

  vg_FILTER_FORMAT_LINEAR, 
  vg_FILTER_FORMAT_PREMULTIPLIED, 
  
  vg_FILTER_CHANNEL_MASK, 
  
  vg_MAX_SCISSOR_RECTS, 
  vg_MAX_DASH_COUNT, 
  vg_MAX_KERNEL_SIZE, 
  vg_MAX_SEPARABLE_KERNEL_SIZE, 
  vg_MAX_COLOR_RAMP_STOPS,
  vg_MAX_IMAGE_WIDTH, 
  vg_MAX_IMAGE_HEIGHT, 
  vg_MAX_IMAGE_PIXELS, 
  vg_MAX_IMAGE_BYTES, 
  vg_MAX_FLOAT,

  -- * Functions
  vgSetf, 
  vgSeti, 
--  vgSetfv, MISSING?
  vgSetiv,
  
  vgGetf, 
  vgGeti, 
  vgGetVectorSize, 
  vgGetfv, 
  vgGetiv,
  
  vgSetParameterf, 
  vgSetParameteri, 
  vgSetParameterfv, 
  vgSetParameteriv,
  
  vgGetParameterf, 
  vgGetParameteri, 
  vgGetParameterVectorSize,
  vgGetParameterfv, 
  vgGetParameteriv
) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101



