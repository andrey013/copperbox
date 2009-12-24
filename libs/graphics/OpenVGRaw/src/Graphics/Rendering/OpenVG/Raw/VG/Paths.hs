{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Paths
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 8 (Paths) 
-- of the OpenVG 1.0.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Paths (
  -- * Tokens
  vg_PATH_FORMAT_STANDARD,

  vg_PATH_DATATYPE_S_8,
  vg_PATH_DATATYPE_S_16,
  vg_PATH_DATATYPE_S_32,
  vg_PATH_DATATYPE_F,

  vg_ABSOLUTE,
  vg_RELATIVE,

  vg_CLOSE_PATH,
  vg_MOVE_TO,
  vg_LINE_TO,
  vg_HLINE_TO,
  vg_VLINE_TO,
  vg_QUAD_TO,
  vg_CUBIC_TO,
  vg_SQUAD_TO,
  vg_SCUBIC_TO,
  vg_SCCWARC_TO,
  vg_SCWARC_TO,
  vg_LCCWARC_TO,
  vg_LCWARC_TO,

  vg_MOVE_TO_ABS,
  vg_MOVE_TO_REL,
  vg_LINE_TO_ABS,
  vg_LINE_TO_REL,
  vg_HLINE_TO_ABS,
  vg_HLINE_TO_REL,
  vg_VLINE_TO_ABS,
  vg_VLINE_TO_REL,
  vg_QUAD_TO_ABS,
  vg_QUAD_TO_REL,
  vg_CUBIC_TO_ABS,
  vg_CUBIC_TO_REL,
  vg_SQUAD_TO_ABS,
  vg_SQUAD_TO_REL,
  vg_SCUBIC_TO_ABS,
  vg_SCUBIC_TO_REL,
  vg_SCCWARC_TO_ABS,
  vg_SCCWARC_TO_REL,
  vg_SCWARC_TO_ABS,
  vg_SCWARC_TO_REL,
  vg_LCCWARC_TO_ABS,
  vg_LCCWARC_TO_REL,
  vg_LCWARC_TO_ABS,
  vg_LCWARC_TO_REL,
  vg_PATH_CAPABILITY_APPEND_FROM,
  vg_PATH_CAPABILITY_APPEND_TO,
  vg_PATH_CAPABILITY_MODIFY,
  vg_PATH_CAPABILITY_TRANSFORM_FROM,
  vg_PATH_CAPABILITY_TRANSFORM_TO,
  vg_PATH_CAPABILITY_INTERPOLATE_FROM,
  vg_PATH_CAPABILITY_INTERPOLATE_TO,
  vg_PATH_CAPABILITY_PATH_LENGTH,
  vg_PATH_CAPABILITY_POINT_ALONG_PATH,
  vg_PATH_CAPABILITY_TANGENT_ALONG_PATH,
  vg_PATH_CAPABILITY_PATH_BOUNDS,
  vg_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS,
  vg_PATH_CAPABILITY_ALL,

  vg_PATH_FORMAT,
  vg_PATH_DATATYPE,
  vg_PATH_SCALE,
  vg_PATH_BIAS,
  vg_PATH_NUM_SEGMENTS,
  vg_PATH_NUM_COORDS,

  vg_CAP_BUTT,
  vg_CAP_ROUND,
  vg_CAP_SQUARE,
 
  vg_JOIN_MITER,
  vg_JOIN_ROUND,
  vg_JOIN_BEVEL,

  vg_EVEN_ODD,
  vg_NON_ZERO,

  vg_STROKE_PATH,
  vg_FILL_PATH,


  -- * Types
  VGPath,

  -- * Functions
  vgCreatePath, 
  vgClearPath, 
  vgDestroyPath, 
  vgGetPathCapabilities, 
  vgRemovePathCapabilities, 
  vgAppendPath, 
  vgAppendPathData, 
  vgModifyPathCoords,
  vgTransformPath, 
  vgInterpolatePath,    
--  vgPathLength,               MISSING?
--  vgPointAlongPath,           MISSING?
--  vgPathBounds,               MISSING?
--  vgPathTRansformedBounds,    MISSING?
  vgDrawPath

) where


import Graphics.Rendering.OpenVG.Raw.VG.Core101
