{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Constants
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Aliases for constants defined in <vg/openvg.h>.
--
--------------------------------------------------------------------------------


module Graphics.Rendering.OpenVG.VG.Constants where

#include <vg/openvg.h>

import Graphics.Rendering.OpenVG.VG.BasicTypes

vg_PATH_FORMAT_STANDARD :: VGint
vg_PATH_FORMAT_STANDARD = #const VG_PATH_FORMAT_STANDARD

vg_MAXSHORT :: VGshort
vg_MAXSHORT = #const VG_MAXSHORT

vg_MAXINT :: VGint
vg_MAXINT = #const VG_MAXINT

-- shiva-vg has no VG_MAXFLOAT
-- vg_MAXFLOAT :: VGfloat
-- vg_MAXFLOAT = #const VG_MAXFLOAT


-- | Enumerations

#{enum VGenum,
  , vg_NO_ERROR                         = VG_NO_ERROR
  , vg_BAD_HANDLE_ERROR                 = VG_BAD_HANDLE_ERROR
  , vg_ILLEGAL_ARGUMENT_ERROR           = VG_ILLEGAL_ARGUMENT_ERROR
  , vg_OUT_OF_MEMORY_ERROR              = VG_OUT_OF_MEMORY_ERROR
  , vg_PATH_CAPABILITY_ERROR            = VG_PATH_CAPABILITY_ERROR
  , vg_UNSUPPORTED_IMAGE_FORMAT_ERROR   = VG_UNSUPPORTED_IMAGE_FORMAT_ERROR
  , vg_UNSUPPORTED_PATH_FORMAT_ERROR    = VG_UNSUPPORTED_PATH_FORMAT_ERROR
  , vg_IMAGE_IN_USE_ERROR               = VG_IMAGE_IN_USE_ERROR
  , vg_NO_CONTEXT_ERROR                 = VG_NO_CONTEXT_ERROR
  }
  
#{enum VGenum,
  , vg_MATRIX_MODE                  = VG_MATRIX_MODE
  , vg_FILL_RULE                    = VG_FILL_RULE
  , vg_IMAGE_QUALITY                = VG_IMAGE_QUALITY
  , vg_RENDERING_QUALITY            = VG_RENDERING_QUALITY
  , vg_BLEND_MODE                   = VG_BLEND_MODE
  , vg_IMAGE_MODE                   = VG_IMAGE_MODE
  
  , vg_SCISSOR_RECTS                = VG_SCISSOR_RECTS

  , vg_STROKE_LINE_WIDTH            = VG_STROKE_LINE_WIDTH
  , vg_STROKE_CAP_STYLE             = VG_STROKE_CAP_STYLE
  , vg_STROKE_JOIN_STYLE            = VG_STROKE_JOIN_STYLE
  , vg_STROKE_MITER_LIMIT           = VG_STROKE_MITER_LIMIT
  , vg_STROKE_DASH_PATTERN          = VG_STROKE_DASH_PATTERN
  , vg_STROKE_DASH_PHASE            = VG_STROKE_DASH_PHASE
  , vg_STROKE_DASH_PHASE_RESET      = VG_STROKE_DASH_PHASE_RESET

  , vg_TILE_FILL_COLOR              = VG_TILE_FILL_COLOR

  , vg_CLEAR_COLOR                  = VG_CLEAR_COLOR

  , vg_MASKING                      = VG_MASKING
  , vg_SCISSORING                   = VG_SCISSORING

  , vg_PIXEL_LAYOUT                 = VG_PIXEL_LAYOUT
  , vg_SCREEN_LAYOUT                = VG_SCREEN_LAYOUT

  , vg_FILTER_FORMAT_LINEAR         = VG_FILTER_FORMAT_LINEAR
  , vg_FILTER_FORMAT_PREMULTIPLIED  = VG_FILTER_FORMAT_PREMULTIPLIED

  , vg_FILTER_CHANNEL_MASK          = VG_FILTER_CHANNEL_MASK

  , vg_MAX_SCISSOR_RECTS            = VG_MAX_SCISSOR_RECTS
  , vg_MAX_DASH_COUNT               = VG_MAX_DASH_COUNT
  , vg_MAX_KERNEL_SIZE              = VG_MAX_KERNEL_SIZE
  , vg_MAX_SEPARABLE_KERNEL_SIZE    = VG_MAX_SEPARABLE_KERNEL_SIZE
  , vg_MAX_COLOR_RAMP_STOPS         = VG_MAX_COLOR_RAMP_STOPS
  , vg_MAX_IMAGE_WIDTH              = VG_MAX_IMAGE_WIDTH
  , vg_MAX_IMAGE_HEIGHT             = VG_MAX_IMAGE_HEIGHT
  , vg_MAX_IMAGE_PIXELS             = VG_MAX_IMAGE_PIXELS
  , vg_MAX_IMAGE_BYTES              = VG_MAX_IMAGE_BYTES
  , vg_MAX_FLOAT                    = VG_MAX_FLOAT
  , vg_MAX_GAUSSIAN_STD_DEVIATION   = VG_MAX_GAUSSIAN_STD_DEVIATION 
  }
    

#{enum VGenum,
  , vg_RENDERING_QUALITY_NONANTIALIASED = VG_RENDERING_QUALITY_NONANTIALIASED
  , vg_RENDERING_QUALITY_FASTER         = VG_RENDERING_QUALITY_FASTER
  , vg_RENDERING_QUALITY_BETTER         = VG_RENDERING_QUALITY_BETTER 
  }   
    


#{enum VGenum,
  , vg_PIXEL_LAYOUT_UNKNOWN             = VG_PIXEL_LAYOUT_UNKNOWN
  , vg_PIXEL_LAYOUT_RGB_VERTICAL        = VG_PIXEL_LAYOUT_RGB_VERTICAL
  , vg_PIXEL_LAYOUT_BGR_VERTICAL        = VG_PIXEL_LAYOUT_BGR_VERTICAL
  , vg_PIXEL_LAYOUT_RGB_HORIZONTAL      = VG_PIXEL_LAYOUT_RGB_HORIZONTAL
  , vg_PIXEL_LAYOUT_BGR_HORIZONTAL      = VG_PIXEL_LAYOUT_BGR_HORIZONTAL
  }

#{enum VGenum,
  , vg_MATRIX_PATH_USER_TO_SURFACE    = VG_MATRIX_PATH_USER_TO_SURFACE
  , vg_MATRIX_IMAGE_USER_TO_SURFACE   = VG_MATRIX_IMAGE_USER_TO_SURFACE
  , vg_MATRIX_FILL_PAINT_TO_USER      = VG_MATRIX_FILL_PAINT_TO_USER
  , vg_MATRIX_STROKE_PAINT_TO_USER    = VG_MATRIX_STROKE_PAINT_TO_USER
  }

#{enum VGenum,
  , vg_CLEAR_MASK       = VG_CLEAR_MASK
  , vg_FILL_MASK        = VG_FILL_MASK
  , vg_SET_MASK         = VG_SET_MASK
  , vg_UNION_MASK       = VG_UNION_MASK
  , vg_INTERSECT_MASK   = VG_INTERSECT_MASK
  , vg_SUBTRACT_MASK    = VG_SUBTRACT_MASK
  }
    
    
#{enum VGenum,
  , vg_PATH_DATATYPE_S_8    = VG_PATH_DATATYPE_S_8
  , vg_PATH_DATATYPE_S_16   = VG_PATH_DATATYPE_S_16
  , vg_PATH_DATATYPE_S_32   = VG_PATH_DATATYPE_S_32
  , vg_PATH_DATATYPE_F      = VG_PATH_DATATYPE_F
  }

#{enum VGenum,
  , vg_ABSOLUTE         = VG_ABSOLUTE
  , vg_RELATIVE         = VG_RELATIVE
  }

#{enum VGenum,
  , vg_CLOSE_PATH       = VG_CLOSE_PATH
  , vg_MOVE_TO          = VG_MOVE_TO
  , vg_LINE_TO          = VG_LINE_TO
  , vg_HLINE_TO         = VG_HLINE_TO
  , vg_VLINE_TO         = VG_VLINE_TO
  , vg_QUAD_TO          = VG_QUAD_TO
  , vg_CUBIC_TO         = VG_CUBIC_TO
  , vg_SQUAD_TO         = VG_SQUAD_TO
  , vg_SCUBIC_TO        = VG_SCUBIC_TO
  , vg_SCCWARC_TO       = VG_SCCWARC_TO
  , vg_SCWARC_TO        = VG_SCWARC_TO
  , vg_LCCWARC_TO       = VG_LCCWARC_TO
  , vg_LCWARC_TO        = VG_LCWARC_TO
  }


#{enum VGenum,
  , vg_MOVE_TO_ABS      = VG_MOVE_TO_ABS
  , vg_MOVE_TO_REL      = VG_MOVE_TO_REL
  , vg_LINE_TO_ABS      = VG_LINE_TO_ABS
  , vg_LINE_TO_REL      = VG_LINE_TO_REL
  , vg_HLINE_TO_ABS     = VG_HLINE_TO_ABS
  , vg_HLINE_TO_REL     = VG_HLINE_TO_REL
  , vg_VLINE_TO_ABS     = VG_VLINE_TO_ABS
  , vg_VLINE_TO_REL     = VG_VLINE_TO_REL
  , vg_QUAD_TO_ABS      = VG_QUAD_TO_ABS
  , vg_QUAD_TO_REL      = VG_QUAD_TO_REL
  , vg_CUBIC_TO_ABS     = VG_CUBIC_TO_ABS
  , vg_CUBIC_TO_REL     = VG_CUBIC_TO_REL
  , vg_SQUAD_TO_ABS     = VG_SQUAD_TO_ABS
  , vg_SQUAD_TO_REL     = VG_SQUAD_TO_REL
  , vg_SCUBIC_TO_ABS    = VG_SCUBIC_TO_ABS
  , vg_SCUBIC_TO_REL    = VG_SCUBIC_TO_REL
  , vg_SCCWARC_TO_ABS   = VG_SCCWARC_TO_ABS
  , vg_SCCWARC_TO_REL   = VG_SCCWARC_TO_REL
  , vg_SCWARC_TO_ABS    = VG_SCWARC_TO_ABS
  , vg_SCWARC_TO_REL    = VG_SCWARC_TO_REL
  , vg_LCCWARC_TO_ABS   = VG_LCCWARC_TO_ABS
  , vg_LCCWARC_TO_REL   = VG_LCCWARC_TO_REL
  , vg_LCWARC_TO_ABS    = VG_LCWARC_TO_ABS
  , vg_LCWARC_TO_REL    = VG_LCWARC_TO_REL
  }

-- typedef VGHandle VGPath;


#{enum VGenum,
  , vg_PATH_CAPABILITY_APPEND_FROM              = VG_PATH_CAPABILITY_APPEND_FROM
  , vg_PATH_CAPABILITY_APPEND_TO                = VG_PATH_CAPABILITY_APPEND_TO
  , vg_PATH_CAPABILITY_MODIFY                   = VG_PATH_CAPABILITY_MODIFY
  , vg_PATH_CAPABILITY_TRANSFORM_FROM           = VG_PATH_CAPABILITY_TRANSFORM_FROM
  , vg_PATH_CAPABILITY_TRANSFORM_TO             = VG_PATH_CAPABILITY_TRANSFORM_TO
  , vg_PATH_CAPABILITY_INTERPOLATE_FROM         = VG_PATH_CAPABILITY_INTERPOLATE_FROM
  , vg_PATH_CAPABILITY_INTERPOLATE_TO           = VG_PATH_CAPABILITY_INTERPOLATE_TO
  , vg_PATH_CAPABILITY_PATH_LENGTH              = VG_PATH_CAPABILITY_PATH_LENGTH
  , vg_PATH_CAPABILITY_POINT_ALONG_PATH         = VG_PATH_CAPABILITY_POINT_ALONG_PATH
  , vg_PATH_CAPABILITY_TANGENT_ALONG_PATH       = VG_PATH_CAPABILITY_TANGENT_ALONG_PATH
  , vg_PATH_CAPABILITY_PATH_BOUNDS              = VG_PATH_CAPABILITY_PATH_BOUNDS
  , vg_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS  = VG_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS
  , vg_PATH_CAPABILITY_ALL                      = VG_PATH_CAPABILITY_ALL
  }

#{enum VGenum,
  , vg_PATH_FORMAT            = VG_PATH_FORMAT
  , vg_PATH_DATATYPE          = VG_PATH_DATATYPE
  , vg_PATH_SCALE             = VG_PATH_SCALE
  , vg_PATH_BIAS              = VG_PATH_BIAS
  , vg_PATH_NUM_SEGMENTS      = VG_PATH_NUM_SEGMENTS
  , vg_PATH_NUM_COORDS        = VG_PATH_NUM_COORDS
  }

#{enum VGenum,
  , vg_CAP_BUTT         = VG_CAP_BUTT
  , vg_CAP_ROUND        = VG_CAP_ROUND
  , vg_CAP_SQUARE       = VG_CAP_SQUARE
  }

#{enum VGenum,
  , vg_JOIN_MITER       = VG_JOIN_MITER
  , vg_JOIN_ROUND       = VG_JOIN_ROUND
  , vg_JOIN_BEVEL       = VG_JOIN_BEVEL
  }

#{enum VGenum,
  , vg_EVEN_ODD         = VG_EVEN_ODD
  , vg_NON_ZERO         = VG_NON_ZERO
  }

#{enum VGenum,
  , vg_STROKE_PATH      = VG_STROKE_PATH
  , vg_FILL_PATH        = VG_FILL_PATH
}

-- typedef VGHandle VGPaint;

#{enum VGenum,
  , vg_PAINT_TYPE                       = VG_PAINT_TYPE
  , vg_PAINT_COLOR                      = VG_PAINT_COLOR
  , vg_PAINT_COLOR_RAMP_SPREAD_MODE     = VG_PAINT_COLOR_RAMP_SPREAD_MODE
  , vg_PAINT_COLOR_RAMP_PREMULTIPLIED   = VG_PAINT_COLOR_RAMP_PREMULTIPLIED
  , vg_PAINT_COLOR_RAMP_STOPS           = VG_PAINT_COLOR_RAMP_STOPS

  , vg_PAINT_LINEAR_GRADIENT            = VG_PAINT_LINEAR_GRADIENT

  , vg_PAINT_RADIAL_GRADIENT            = VG_PAINT_RADIAL_GRADIENT

  , vg_PAINT_PATTERN_TILING_MODE        = VG_PAINT_PATTERN_TILING_MODE
}

#{enum VGenum,
  , vg_PAINT_TYPE_COLOR                 = VG_PAINT_TYPE_COLOR
  , vg_PAINT_TYPE_LINEAR_GRADIENT       = VG_PAINT_TYPE_LINEAR_GRADIENT
  , vg_PAINT_TYPE_RADIAL_GRADIENT       = VG_PAINT_TYPE_RADIAL_GRADIENT
  , vg_PAINT_TYPE_PATTERN               = VG_PAINT_TYPE_PATTERN
  }

#{enum VGenum,
  , vg_COLOR_RAMP_SPREAD_PAD            = VG_COLOR_RAMP_SPREAD_PAD
  , vg_COLOR_RAMP_SPREAD_REPEAT         = VG_COLOR_RAMP_SPREAD_REPEAT
  , vg_COLOR_RAMP_SPREAD_REFLECT        = VG_COLOR_RAMP_SPREAD_REFLECT
  }

#{enum VGenum,
  , vg_TILE_FILL        = VG_TILE_FILL
  , vg_TILE_PAD         = VG_TILE_PAD
  , vg_TILE_REPEAT      = VG_TILE_REPEAT
  , vg_TILE_REFLECT     = VG_TILE_REFLECT
  }

#{enum VGenum,
  , vg_sRGBX_8888       = VG_sRGBX_8888
  , vg_sRGBA_8888       = VG_sRGBA_8888
  , vg_sRGBA_8888_PRE   = VG_sRGBA_8888_PRE
  , vg_sRGB_565         = VG_sRGB_565
  , vg_sRGBA_5551       = VG_sRGBA_5551
  , vg_sRGBA_4444       = VG_sRGBA_4444
  , vg_sL_8             = VG_sL_8
  , vg_lRGBX_8888       = VG_lRGBX_8888
  , vg_lRGBA_8888       = VG_lRGBA_8888
  , vg_lRGBA_8888_PRE   = VG_lRGBA_8888_PRE
  , vg_lL_8             = VG_lL_8
  , vg_A_8              = VG_A_8
  , vg_BW_1             = VG_BW_1

  , vg_sXRGB_8888       = VG_sXRGB_8888
  , vg_sARGB_8888       = VG_sARGB_8888
  , vg_sARGB_8888_PRE   = VG_sARGB_8888_PRE
  , vg_sARGB_1555       = VG_sARGB_1555
  , vg_sARGB_4444       = VG_sARGB_4444
  , vg_lXRGB_8888       = VG_lXRGB_8888
  , vg_lARGB_8888       = VG_lARGB_8888
  , vg_lARGB_8888_PRE   = VG_lARGB_8888_PRE

  , vg_sBGRX_8888       = VG_sBGRX_8888
  , vg_sBGRA_8888       = VG_sBGRA_8888
  , vg_sBGRA_8888_PRE   = VG_sBGRA_8888_PRE
  , vg_sBGR_565         = VG_sBGR_565
  , vg_sBGRA_5551       = VG_sBGRA_5551
  , vg_sBGRA_4444       = VG_sBGRA_4444
  , vg_lBGRX_8888       = VG_lBGRX_8888
  , vg_lBGRA_8888       = VG_lBGRA_8888
  , vg_lBGRA_8888_PRE   = VG_lBGRA_8888_PRE

  , vg_sXBGR_8888       = VG_sXBGR_8888
  , vg_sABGR_8888       = VG_sABGR_8888
  , vg_sABGR_8888_PRE   = VG_sABGR_8888_PRE
  , vg_sABGR_1555       = VG_sABGR_1555
  , vg_sABGR_4444       = VG_sABGR_4444
  , vg_lXBGR_8888       = VG_lXBGR_8888
  , vg_lABGR_8888       = VG_lABGR_8888
  , vg_lABGR_8888_PRE   = VG_lABGR_8888_PRE
}


-- typedef VGHandle VGImage;


#{enum VGenum,
  , vg_IMAGE_QUALITY_NONANTIALIASED     = VG_IMAGE_QUALITY_NONANTIALIASED
  , vg_IMAGE_QUALITY_FASTER             = VG_IMAGE_QUALITY_FASTER
  , vg_IMAGE_QUALITY_BETTER             = VG_IMAGE_QUALITY_BETTER
  }

#{enum VGenum,
  , vg_IMAGE_FORMAT     = VG_IMAGE_FORMAT
  , vg_IMAGE_WIDTH      = VG_IMAGE_WIDTH
  , vg_IMAGE_HEIGHT     = VG_IMAGE_HEIGHT
  }

#{enum VGenum,
  , vg_DRAW_IMAGE_NORMAL      = VG_DRAW_IMAGE_NORMAL
  , vg_DRAW_IMAGE_MULTIPLY    = VG_DRAW_IMAGE_MULTIPLY
  , vg_DRAW_IMAGE_STENCIL     = VG_DRAW_IMAGE_STENCIL
  }

#{enum VGenum,
  , vg_RED              = VG_RED
  , vg_GREEN            = VG_GREEN
  , vg_BLUE             = VG_BLUE
  , vg_ALPHA            = VG_ALPHA
  }

#{enum VGenum,
  , vg_BLEND_SRC              = VG_BLEND_SRC
  , vg_BLEND_SRC_OVER         = VG_BLEND_SRC_OVER
  , vg_BLEND_DST_OVER         = VG_BLEND_DST_OVER
  , vg_BLEND_SRC_IN           = VG_BLEND_SRC_IN
  , vg_BLEND_DST_IN           = VG_BLEND_DST_IN
  , vg_BLEND_MULTIPLY         = VG_BLEND_MULTIPLY
  , vg_BLEND_SCREEN           = VG_BLEND_SCREEN
  , vg_BLEND_DARKEN           = VG_BLEND_DARKEN
  , vg_BLEND_LIGHTEN          = VG_BLEND_LIGHTEN
  , vg_BLEND_ADDITIVE         = VG_BLEND_ADDITIVE
  , vg_BLEND_SRC_OUT_SH       = VG_BLEND_SRC_OUT_SH
  , vg_BLEND_DST_OUT_SH       = VG_BLEND_DST_OUT_SH
  , vg_BLEND_SRC_ATOP_SH      = VG_BLEND_SRC_ATOP_SH
  , vg_BLEND_DST_ATOP_SH      = VG_BLEND_DST_ATOP_SH
  }

#{enum VGenum,
  , vg_IMAGE_FORMAT_QUERY     = VG_IMAGE_FORMAT_QUERY
  , vg_PATH_DATATYPE_QUERY    = VG_PATH_DATATYPE_QUERY
  }

#{enum VGenum,
  , vg_HARDWARE_ACCELERATED   = VG_HARDWARE_ACCELERATED
  , vg_HARDWARE_UNACCELERATED = VG_HARDWARE_UNACCELERATED
  }

#{enum VGenum,
  , vg_VENDOR           = VG_VENDOR
  , vg_RENDERER         = VG_RENDERER
  , vg_VERSION          = VG_VERSION
  , vg_EXTENSIONS       = VG_EXTENSIONS
  }

-- end of file

