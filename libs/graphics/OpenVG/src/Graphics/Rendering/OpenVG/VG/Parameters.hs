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
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Parameters (    
    ParamType(..), marshalParamType,
    setf, seti, setfv, setiv,
    getf, geti, getVectorSize, getfv, getiv,
    setParameterf, setParameteri, setParameterfv, setParameteriv,
    getParameterf, getParameteri, getParameterVectorSize,
    getParameterfv, getParameteriv
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGfloat, VGint, VGHandle )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgSetf, vgSeti, vgSetfv, vgSetiv,
    vgGetf, vgGeti, vgGetVectorSize, vgGetfv, vgGetiv,
     
    vgSetParameterf, vgSetParameteri, vgSetParameterfv, vgSetParameteriv, 
    vgGetParameterf, vgGetParameteri, vgGetParameterVectorSize,
    vgGetParameterfv, vgGetParameteriv )
    
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

import Foreign.Marshal.Array ( newArray, peekArray ) 


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

setf :: ParamType -> VGfloat -> IO ()
setf typ val = vgSetf (marshalParamType typ) val

seti :: ParamType -> VGint -> IO ()
seti typ val = vgSeti (marshalParamType typ) val

-- vgSetfv :: VGenum -> VGint -> Ptr VGfloat -> IO ()
-- TODO - Lists or arrays?
setfv :: ParamType -> [VGfloat] -> IO ()
setfv typ vals = do
    a <- newArray vals
    vgSetfv (marshalParamType typ) (fromIntegral $ length vals) a
    
setiv :: ParamType -> [VGint] -> IO ()
setiv typ vals = do
    a <- newArray vals
    vgSetiv (marshalParamType typ) (fromIntegral $ length vals) a

        
getf :: ParamType -> IO VGfloat
getf typ = vgGetf (marshalParamType typ)

geti :: ParamType -> IO VGint
geti typ = vgGeti (marshalParamType typ)


getVectorSize :: ParamType -> IO VGint
getVectorSize typ = vgGetVectorSize (marshalParamType typ)

getfv :: ParamType -> VGint -> IO [VGfloat]
getfv typ i = do
    ptr <- vgGetfv (marshalParamType typ) i 
    peekArray (fromIntegral i) ptr
    
getiv :: ParamType -> VGint -> IO [VGint]
getiv typ i = do
    ptr <- vgGetiv (marshalParamType typ) i 
    peekArray (fromIntegral i) ptr


setParameterf :: VGHandle -> VGenum -> VGfloat -> IO ()
setParameterf = vgSetParameterf
                                 
setParameteri :: VGHandle -> VGenum -> VGint -> IO ()
setParameteri = vgSetParameteri

setParameterfv :: VGHandle -> VGenum -> [VGfloat] -> IO ()
setParameterfv h typ vals = do
    a <- newArray vals
    vgSetParameterfv h typ (fromIntegral $ length vals) a

setParameteriv :: VGHandle -> VGenum -> [VGint] -> IO ()
setParameteriv h typ vals = do
    a <- newArray vals
    vgSetParameteriv h typ (fromIntegral $ length vals) a

getParameterf :: VGHandle -> VGenum -> IO VGfloat
getParameterf = vgGetParameterf

getParameteri :: VGHandle -> VGenum -> IO VGint
getParameteri = vgGetParameteri


getParameterVectorSize :: VGHandle -> VGenum -> IO VGint
getParameterVectorSize = vgGetParameterVectorSize

getParameterfv :: VGHandle -> VGenum -> VGint -> IO [VGfloat]
getParameterfv h typ i = do
    ptr <- vgGetParameterfv h typ i 
    peekArray (fromIntegral i) ptr

getParameteriv :: VGHandle -> VGenum -> VGint -> IO [VGint]
getParameteriv h typ i = do
    ptr <- vgGetParameteriv h typ i 
    peekArray (fromIntegral i) ptr
                
--------------------------------------------------------------------------------    
    
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



