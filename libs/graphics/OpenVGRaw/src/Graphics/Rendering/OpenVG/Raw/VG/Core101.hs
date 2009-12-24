{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.Raw.VG.Core101
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- All the raw functions and types from the OpenVG 1.0.1 core 
-- specs (implemented by ShivaVG-0.2.1)
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.Raw.VG.Core101 (

  -- BasicTypes.hsc
  -- * Primitive Types
  VGbyte, 
  VGubyte, 
  VGshort, 
  VGint, 
  VGuint, 
  VGbitfield, 
  VGboolean, 
  VGfloat,
  
  VGenum,
  
  -- * Handle-based Types
  VGHandle,
  
  VGPath, 
  VGImage, 
  VGPaint,

  -- Constants.hsc
  -- Section 3 - Constants
  vg_OPENVG_VERSION_1_0_1,
  
  vg_FALSE, 
  vg_TRUE,
  
  vg_MAXSHORT,
  vg_MAXINT,
 
  vg_INVALID_HANDLE,

  -- Section 4 - Drawing Context
  vg_NO_ERROR,
  vg_BAD_HANDLE_ERROR,
  vg_ILLEGAL_ARGUMENT_ERROR,
  vg_OUT_OF_MEMORY_ERROR,
  vg_PATH_CAPABILITY_ERROR,
  vg_UNSUPPORTED_IMAGE_FORMAT_ERROR,
  vg_UNSUPPORTED_PATH_FORMAT_ERROR,
  vg_IMAGE_IN_USE_ERROR,
  vg_NO_CONTEXT_ERROR,


  -- Section 5 - Setting API Parameters
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
  vg_MAX_GAUSSIAN_STD_DEVIATION,

  -- Section 6 - Rendering Quality and Antialiasing
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

  -- Section 7 - Scissoring
  vg_CLEAR_MASK,
  vg_FILL_MASK,
  vg_SET_MASK,
  vg_UNION_MASK,
  vg_INTERSECT_MASK,
  vg_SUBTRACT_MASK,
    
  -- Section 8 - Paths
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

  -- Section 9 - Paint
  vg_PAINT_TYPE,
  vg_PAINT_COLOR,
  vg_PAINT_COLOR_RAMP_SPREAD_MODE,
  vg_PAINT_COLOR_RAMP_PREMULTIPLIED,
  vg_PAINT_COLOR_RAMP_STOPS,

  vg_PAINT_LINEAR_GRADIENT,

  vg_PAINT_RADIAL_GRADIENT,

  vg_PAINT_PATTERN_TILING_MODE,


  vg_PAINT_TYPE_COLOR,
  vg_PAINT_TYPE_LINEAR_GRADIENT,
  vg_PAINT_TYPE_RADIAL_GRADIENT,
  vg_PAINT_TYPE_PATTERN,


  vg_COLOR_RAMP_SPREAD_PAD,
  vg_COLOR_RAMP_SPREAD_REPEAT,
  vg_COLOR_RAMP_SPREAD_REFLECT,


  vg_TILE_FILL,
  vg_TILE_PAD,
  vg_TILE_REPEAT,
  vg_TILE_REFLECT,

  -- Section 10 - Images
  vg_sRGBX_8888,
  vg_sRGBA_8888,
  vg_sRGBA_8888_PRE,
  vg_sRGB_565,
  vg_sRGBA_5551,
  vg_sRGBA_4444,
  vg_sL_8,
  vg_lRGBX_8888,
  vg_lRGBA_8888,
  vg_lRGBA_8888_PRE,
  vg_lL_8,
  vg_A_8,
  vg_BW_1,

  vg_sXRGB_8888,
  vg_sARGB_8888,
  vg_sARGB_8888_PRE,
  vg_sARGB_1555,
  vg_sARGB_4444,
  vg_lXRGB_8888,
  vg_lARGB_8888,
  vg_lARGB_8888_PRE,

  vg_sBGRX_8888,
  vg_sBGRA_8888,
  vg_sBGRA_8888_PRE,
  vg_sBGR_565,
  vg_sBGRA_5551,
  vg_sBGRA_4444,
  vg_lBGRX_8888,
  vg_lBGRA_8888,
  vg_lBGRA_8888_PRE,

  vg_sXBGR_8888,
  vg_sABGR_8888,
  vg_sABGR_8888_PRE,
  vg_sABGR_1555,
  vg_sABGR_4444,
  vg_lXBGR_8888,
  vg_lABGR_8888,
  vg_lABGR_8888_PRE,


  vg_IMAGE_QUALITY_NONANTIALIASED,
  vg_IMAGE_QUALITY_FASTER,
  vg_IMAGE_QUALITY_BETTER,

  vg_IMAGE_FORMAT,
  vg_IMAGE_WIDTH,
  vg_IMAGE_HEIGHT,

  vg_DRAW_IMAGE_NORMAL,
  vg_DRAW_IMAGE_MULTIPLY,
  vg_DRAW_IMAGE_STENCIL,
 
  -- Section 11 - Image Filters
  vg_RED,
  vg_GREEN,
  vg_BLUE,
  vg_ALPHA,

  -- Section 12 - Blending
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
  vg_BLEND_DST_ATOP_SH,
  
  -- Section 13 - Querying Hardware Capabilities
  vg_IMAGE_FORMAT_QUERY,
  vg_PATH_DATATYPE_QUERY,
  
  vg_HARDWARE_ACCELERATED,
  vg_HARDWARE_UNACCELERATED,
 
  -- Section 14 - Extending the API
  vg_VENDOR,
  vg_RENDERER,
  vg_VERSION,
  vg_EXTENSIONS,

  -- CFunDecls.hsc
  -- * Functions
  -- Section 4 - Drawing Context
  vgGetError,
  vgFlush,
  vgFinish,
  
  -- Section 5 - Setting API parameters
  vgSeti,
  vgSetf,
  vgSetfv,
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
  vgGetParameteriv,
  
  -- Section 6 - Rendering Quality
  vgLoadIdentity,
  vgLoadMatrix,
  vgGetMatrix,
  vgMultMatrix,
  vgTranslate,
  vgScale,
  vgShear,
  vgRotate,
  
  -- Section 7 - Scissoring
  vgMask,
  vgClear,
  
  -- Section 8 - Paths
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
  vgPathLength,
  vgPointAlongPath,
  vgPathBounds,
  vgPathTransformedBounds,
  vgDrawPath,
  
  -- Section 9 - Paint
  vgCreatePaint,
  vgDestroyPaint,
  vgSetPaint,
--  vgGetPaint,         NOT IMPLEMENTED BY SHIVA-VG
  vgPaintPattern,
  
  -- Section 10 - Images
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
  vgCopyPixels,
  
  -- Section 11 - Image Filters
  vgColorMatrix,
  vgConvolve,
  vgSeparableConvolve,
  vgGaussianBlur,
  vgLookup,
  vgLookupSingle,
  
  -- Section 13 - Querying Harware
  vgHardwareQuery,
  vgGetString,
  
  -- ShivaVG extensions
  vgCreateContextSH,
  vgResizeSurfaceSH,
  vgDestroyContextSH




) where


import Graphics.Rendering.OpenVG.Raw.VG.BasicTypes
import Graphics.Rendering.OpenVG.Raw.VG.Constants
import Graphics.Rendering.OpenVG.Raw.VG.CFunDecls

