{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.RenderingQuality
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 6 (Rendering Quality and Antialiasing) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.RenderingQuality (
  -- * Tokens
  vg_RENDERING_QUALITY_NONANTIALIASED, 
  vg_RENDERING_QUALITY_FASTER,
  vg_RENDERING_QUALITY_BETTER,

  vg_PIXEL_LAYOUT_UNKNOWN, 
  vg_PIXEL_LAYOUT_RGB_VERTICAL, 
  vg_PIXEL_LAYOUT_BGR_VERTICAL,
  vg_PIXEL_LAYOUT_RGB_HORIZONTAL, 
  vg_PIXEL_LAYOUT_BGR_HORIZONTAL,

  vg_MATRIX_PATH_USER_TO_SURFACE, 
  vg_MATRIX_IMAGE_USER_TO_SURFACE, 
  vg_MATRIX_FILL_PAINT_TO_USER, 
  vg_MATRIX_STROKE_PAINT_TO_USER, 

  -- * Functions
  vgLoadIdentity,
  vgLoadMatrix,
  vgGetMatrix,
  vgMultMatrix,
  vgTranslate,
  vgScale,
  vgShear,
  vgRotate 

) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101
    

