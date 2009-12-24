{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Blending
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 12 (Blending) 
-- of the OpenVG 1.0.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Blending (
  -- * Tokens
  vg_BLEND_SRC,
  vg_BLEND_SRC_OVER,
  vg_BLEND_DST_OVER,
  vg_BLEND_SRC_IN,
  vg_BLEND_DST_IN,
  vg_BLEND_MULTIPLY,
  vg_BLEND_SCREEN,
  vg_BLEND_DARKEN,
  vg_BLEND_LIGHTEN,
  vg_BLEND_ADDITIVE,
  vg_BLEND_SRC_OUT_SH,
  vg_BLEND_DST_OUT_SH,
  vg_BLEND_SRC_ATOP_SH,
  vg_BLEND_DST_ATOP_SH

) where

import Graphics.Rendering.OpenVG.Raw.VG.Core101


