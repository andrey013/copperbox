{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Parameters
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 5 (Setting API Parameters) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Parameters where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( vgSeti )
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_MATRIX_MODE, vg_FILL_RULE, vg_IMAGE_QUALITY, vg_RENDERING_QUALITY,
    vg_BLEND_MODE, vg_IMAGE_MODE, vg_SCISSOR_RECTS, vg_STROKE_LINE_WIDTH,
    vg_STROKE_CAP_STYLE, vg_STROKE_JOIN_STYLE, vg_STROKE_MITER_LIMIT,
    vg_STROKE_DASH_PATTERN, vg_STROKE_DASH_PHASE, vg_STROKE_DASH_PHASE_RESET,
    vg_TILE_FILL_COLOR, vg_CLEAR_COLOR, vg_MASKING,
    vg_SCISSORING, vg_PIXEL_LAYOUT, vg_SCREEN_LAYOUT, vg_FILTER_FORMAT_LINEAR,
    vg_FILTER_FORMAT_PREMULTIPLIED, vg_FILTER_CHANNEL_MASK, 
    vg_MAX_SCISSOR_RECTS, vg_MAX_DASH_COUNT, vg_MAX_KERNEL_SIZE,
    vg_MAX_SEPARABLE_KERNEL_SIZE, vg_MAX_COLOR_RAMP_STOPS,
    vg_MAX_IMAGE_WIDTH, vg_MAX_IMAGE_HEIGHT, vg_MAX_IMAGE_PIXELS,
    vg_MAX_IMAGE_BYTES, vg_MAX_FLOAT, vg_MAX_GAUSSIAN_STD_DEVIATION )




data ParamType = 
     ParamMatrixMode
   | ParamFillRule
   | ParamImageQuality
   | ParamRenderingQuality
   | ParamBlendMode
   | ParamImageMode
   | ParamScissorRects
   | ParamStrokeLineWidth
   | ParamStrokeCapStyle
   | ParamStrokeJoinStyle
   | ParamStrokeMiterLimit
   | ParamStrokeDashPattern
   | ParamStrokeDashPhase
   | ParamStrokeDashPhaseReset
   | ParamTileFillColor
   | ParamClearColor
   -- | ParamGlyphOrigin        {- Not in shiva-vg -} 
   | ParamMasking
   | ParamScissoring
   | ParamPixelLayout
   | ParamScreenLayout
   | ParamFilterFormatLinear
   | ParamFilterFormatPremultiplied
   | ParamFilterChannelMask
   | ParamMaxScissorRects
   | ParamMaxDashCount
   | ParamMaxKernelSize
   | ParamMaxSaparableKernelSize
   | ParamMaxColorRampStops
   | ParamMaxImageWidth
   | ParamMaxImageHeight
   | ParamMaxImagePixels
   | ParamMaxImageBytes
   | ParamMaxFloat
   | ParamMaxGaussianStdDeviation
   deriving ( Eq, Ord, Show )

marshalParamType :: ParamType -> VGenum
marshalParamType x = case x of
    ParamMatrixMode -> vg_MATRIX_MODE 
    ParamFillRule -> vg_FILL_RULE
    ParamImageQuality -> vg_IMAGE_QUALITY
    ParamRenderingQuality -> vg_RENDERING_QUALITY
    ParamBlendMode -> vg_BLEND_MODE
    ParamImageMode -> vg_IMAGE_MODE
    ParamScissorRects -> vg_SCISSOR_RECTS
    ParamStrokeLineWidth -> vg_STROKE_LINE_WIDTH
    ParamStrokeCapStyle -> vg_STROKE_CAP_STYLE
    ParamStrokeJoinStyle -> vg_STROKE_JOIN_STYLE
    ParamStrokeMiterLimit -> vg_STROKE_MITER_LIMIT
    ParamStrokeDashPattern -> vg_STROKE_DASH_PATTERN
    ParamStrokeDashPhase -> vg_STROKE_DASH_PHASE 
    ParamStrokeDashPhaseReset -> vg_STROKE_DASH_PHASE_RESET
    ParamTileFillColor -> vg_TILE_FILL_COLOR 
    ParamClearColor -> vg_CLEAR_COLOR 
    -- ParamGlyphOrigin -> vg_GLYPH_ORIGIN         {- Not in shiva-vg -}
    ParamMasking -> vg_MASKING
    ParamScissoring -> vg_SCISSORING 
    ParamPixelLayout -> vg_PIXEL_LAYOUT
    ParamScreenLayout -> vg_SCREEN_LAYOUT 
    ParamFilterFormatLinear -> vg_FILTER_FORMAT_LINEAR
    ParamFilterFormatPremultiplied -> vg_FILTER_FORMAT_PREMULTIPLIED
    ParamFilterChannelMask -> vg_FILTER_CHANNEL_MASK 
    ParamMaxScissorRects -> vg_MAX_SCISSOR_RECTS 
    ParamMaxDashCount -> vg_MAX_DASH_COUNT
    ParamMaxKernelSize -> vg_MAX_KERNEL_SIZE
    ParamMaxSaparableKernelSize -> vg_MAX_SEPARABLE_KERNEL_SIZE
    ParamMaxColorRampStops -> vg_MAX_COLOR_RAMP_STOPS
    ParamMaxImageWidth -> vg_MAX_IMAGE_WIDTH
    ParamMaxImageHeight -> vg_MAX_IMAGE_HEIGHT
    ParamMaxImagePixels -> vg_MAX_IMAGE_PIXELS
    ParamMaxImageBytes -> vg_MAX_IMAGE_BYTES
    ParamMaxFloat -> vg_MAX_FLOAT
    ParamMaxGaussianStdDeviation -> vg_MAX_GAUSSIAN_STD_DEVIATION 

--------------------------------------------------------------------------------



