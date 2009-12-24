{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Images
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 10 (Images) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Images (
  -- * Tokens
  vg_IMAGE_QUALITY_NONANTIALIASED, vg_IMAGE_QUALITY_FASTER, 
  vg_IMAGE_QUALITY_BETTER, 

  vg_IMAGE_FORMAT, vg_IMAGE_WIDTH, vg_IMAGE_HEIGHT,

  vg_DRAW_IMAGE_NORMAL, vg_DRAW_IMAGE_MULTIPLY, vg_DRAW_IMAGE_STENCIL,

  vg_sRGBX_8888, vg_sRGBA_8888, vg_sRGBA_8888_PRE, vg_sRGB_565,
  vg_sRGBA_5551, vg_sRGBA_4444, vg_sL_8, vg_lRGBX_8888,
  vg_lRGBA_8888, vg_lRGBA_8888_PRE, vg_lL_8, vg_A_8, vg_BW_1,

  vg_sXRGB_8888, vg_sARGB_8888, vg_sARGB_8888_PRE, vg_sARGB_1555,
  vg_sARGB_4444, vg_lXRGB_8888, vg_lARGB_8888, vg_lARGB_8888_PRE,
  vg_sBGRX_8888, vg_sBGRA_8888, vg_sBGRA_8888_PRE, vg_sBGR_565,
  vg_sBGRA_5551, vg_sBGRA_4444, vg_lBGRX_8888, vg_lBGRA_8888,
  vg_lBGRA_8888_PRE, vg_sXBGR_8888, vg_sABGR_8888,
  vg_sABGR_8888_PRE, vg_sABGR_1555, vg_sABGR_4444, vg_lXBGR_8888,
  vg_lABGR_8888, vg_lABGR_8888_PRE,

  -- * Functions
  vgCreateImage,
  vgDestroyImage,
  vgClearImage,
  vgImageSubData,
  vgGetImageSubData,
  vgChildImage,
  vgGetParent,
  vgCopyImage,
  vgDrawImage,
  vgSetPixels,
  vgWritePixels,
  vgGetPixels,
  vgReadPixels,
  vgCopyPixels



) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101
