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
     MatrixMode
   | FillRule
   | ImageQuality
   | RenderingQuality
   | BlendMode
   | ImageMode
   | ScissorRects
   | StrokeLineWidth
   | StrokeCapStyle
   | StrokeJoinStyle
   | StrokeMiterLimit
   | StrokeDashPattern
   | StrokeDashPhase
   | StrokeDashPhaseReset
   | TileFillColor
   | ClearColor
   -- | GlyphOrigin        {- Not in shiva-vg -} 
   | Masking
   | Scissoring
   | PixelLayout
   | ScreenLayout
   | FilterFormatLinear
   | FilterFormatPremultiplied
   | FilterChannelMask
   | MaxScissorRects
   | MaxDashCount
   | MaxKernelSize
   | MaxSaparableKernelSize
   | MaxColorRampStops
   | MaxImageWidth
   | MaxImageHeight
   | MaxImagePixels
   | MaxImageBytes
   | MaxFloat
   | MaxGaussianStdDeviation
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
    MatrixMode -> vg_MATRIX_MODE 
    FillRule -> vg_FILL_RULE
    ImageQuality -> vg_IMAGE_QUALITY
    RenderingQuality -> vg_RENDERING_QUALITY
    BlendMode -> vg_BLEND_MODE
    ImageMode -> vg_IMAGE_MODE
    ScissorRects -> vg_SCISSOR_RECTS
    StrokeLineWidth -> vg_STROKE_LINE_WIDTH
    StrokeCapStyle -> vg_STROKE_CAP_STYLE
    StrokeJoinStyle -> vg_STROKE_JOIN_STYLE
    StrokeMiterLimit -> vg_STROKE_MITER_LIMIT
    StrokeDashPattern -> vg_STROKE_DASH_PATTERN
    StrokeDashPhase -> vg_STROKE_DASH_PHASE 
    StrokeDashPhaseReset -> vg_STROKE_DASH_PHASE_RESET
    TileFillColor -> vg_TILE_FILL_COLOR 
    ClearColor -> vg_CLEAR_COLOR 
    -- ParamGlyphOrigin -> vg_GLYPH_ORIGIN         {- Not in shiva-vg -}
    Masking -> vg_MASKING
    Scissoring -> vg_SCISSORING 
    PixelLayout -> vg_PIXEL_LAYOUT
    ScreenLayout -> vg_SCREEN_LAYOUT 
    FilterFormatLinear -> vg_FILTER_FORMAT_LINEAR
    FilterFormatPremultiplied -> vg_FILTER_FORMAT_PREMULTIPLIED
    FilterChannelMask -> vg_FILTER_CHANNEL_MASK 
    MaxScissorRects -> vg_MAX_SCISSOR_RECTS 
    MaxDashCount -> vg_MAX_DASH_COUNT
    MaxKernelSize -> vg_MAX_KERNEL_SIZE
    MaxSaparableKernelSize -> vg_MAX_SEPARABLE_KERNEL_SIZE
    MaxColorRampStops -> vg_MAX_COLOR_RAMP_STOPS
    MaxImageWidth -> vg_MAX_IMAGE_WIDTH
    MaxImageHeight -> vg_MAX_IMAGE_HEIGHT
    MaxImagePixels -> vg_MAX_IMAGE_PIXELS
    MaxImageBytes -> vg_MAX_IMAGE_BYTES
    MaxFloat -> vg_MAX_FLOAT
    MaxGaussianStdDeviation -> vg_MAX_GAUSSIAN_STD_DEVIATION 

--------------------------------------------------------------------------------



