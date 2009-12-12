{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Parameters
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

-- | The parameter type of variables accessible with @vgSet@ 
-- or @vgGet@.
-- 
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

-- | Set a value of type 'VGfloat' within the current context.
--
-- 'setf' corresponds to the OpenVG function @vgSetf@.
--
-- > void vgSetf (VGParamType paramType, VGfloat value);
--
setf :: ParamType -> VGfloat -> IO ()
setf typ = vgSetf $ marshalParamType typ

-- | Set a value of type 'VGint' within the current context.
--
-- 'seti' corresponds to the OpenVG function @vgSeti@.
--
-- > void vgSeti (VGParamType paramType, VGint value);
--
seti :: ParamType -> VGint -> IO ()
seti typ = vgSeti $ marshalParamType typ

-- | Set a list of 'VGfloat' values within the current context.
--
-- 'setfv' corresponds to the OpenVG function @vgSetfv@.
--
-- > void vgSetfv(VGParamType paramType, VGint count, const VGfloat * values);
--
setfv :: ParamType -> [VGfloat] -> IO ()
setfv typ vals = newArray vals >>= 
                 vgSetfv (marshalParamType typ) (fromIntegral $ length vals)
    

-- | Set a list of 'VGint' values within the current context.
--
-- 'setiv' corresponds to the OpenVG function @vgSetiv@.
--
-- > void vgSetiv(VGParamType paramType, VGint count, const VGint * values);
--
setiv :: ParamType -> [VGint] -> IO ()
setiv typ vals = newArray vals >>= 
                 vgSetiv (marshalParamType typ) (fromIntegral $ length vals)




-- | Get a value of type 'VGfloat' from the current context.
--
-- 'getf' corresponds to the OpenVG function @vgGetf@.
--
-- > VGfloat vgGetf (VGParamType paramType);
--        
getf :: ParamType -> IO VGfloat
getf = vgGetf . marshalParamType

-- | Get a value of type 'VGint' from the current context.
--
-- 'geti' corresponds to the OpenVG function @vgGeti@.
--
-- > VGint vgGeti (VGParamType paramType);
-- 
geti :: ParamType -> IO VGint
geti = vgGeti . marshalParamType


-- | Get the maximum number of values that the respective 'getfv'
-- or 'getiv' call would return for the given 'ParamType'.
--
-- 'getVectorSize' corresponds to the OpenVG function 
-- @vgGetVectorSize@.
--
-- > VGint vgGetVectorSize(VGParamType paramType);
-- 
getVectorSize :: ParamType -> IO VGint
getVectorSize typ = vgGetVectorSize $ marshalParamType typ


-- | Get a list of value of type 'VGfloat' from the current 
-- context.
--
-- 'getfv' corresponds to the OpenVG function @vgGetfv@.
--
-- > void vgGetfv(VGParamType paramType, VGint count, VGfloat * values);
-- 
getfv :: ParamType -> VGint -> IO [VGfloat]
getfv typ i = vgGetfv (marshalParamType typ) i >>= peekArray (fromIntegral i)
    
-- | Get a list of value of type 'VGint' from the current 
-- context.
--
-- 'getiv' corresponds to the OpenVG function @vgGetiv@.
--
-- > void vgGetiv(VGParamType paramType, VGint count, VGint * values);
-- 
getiv :: ParamType -> VGint -> IO [VGint]
getiv typ i = vgGetiv (marshalParamType typ) i >>= peekArray (fromIntegral i)


-- | Set the supplied parameter of the handle to a value of type
-- 'VGfloat'.
--
-- 'setParameterf' corresponds to the OpenVG function 
-- @vgSetParameterf@.
--
-- > void vgSetParameterf (VGHandle object, VGint paramType, VGfloat value);
--
setParameterf :: VGHandle -> VGenum -> VGfloat -> IO ()
setParameterf = vgSetParameterf


-- | Set the supplied parameter of the handle to a value of type
-- 'VGint'.
--
-- 'setParameteri' corresponds to the OpenVG function 
-- @vgSetParameteri@.
--
-- > void vgSetParameteri (VGHandle object, VGint paramType, VGint value);
--                                 
setParameteri :: VGHandle -> VGenum -> VGint -> IO ()
setParameteri = vgSetParameteri


-- | Set the supplied parameter of the handle to the list of 
-- supplied 'VGfloat' values.
--
-- 'setParameterfv' corresponds to the OpenVG function 
-- @vgSetParameterfv@.
--
-- > void vgSetParameterfv(VGHandle object, VGint paramType, 
-- >                       VGint count, const VGfloat * values);
--
setParameterfv :: VGHandle -> VGenum -> [VGfloat] -> IO ()
setParameterfv h typ vals = 
    newArray vals >>= vgSetParameterfv h typ (fromIntegral $ length vals)


-- | Set the supplied parameter of the handle to the list of 
-- supplied 'VGint' values.
--
-- 'setParameteriv' corresponds to the OpenVG function 
-- @vgSetParameteriv@.
--
-- > void vgSetParameteriv(VGHandle object, VGint paramType, 
-- >                       VGint count, const VGint * values);
--
setParameteriv :: VGHandle -> VGenum -> [VGint] -> IO ()
setParameteriv h typ vals =
    newArray vals >>= vgSetParameteriv h typ (fromIntegral $ length vals)



-- | Get the 'VGfloat' value of the parameter on the supplied 
-- handle.
--
-- 'getParameterf' corresponds to the OpenVG function 
-- @vgGetParameterf@.
--
-- > VGfloat vgGetParameterf (VGHandle object, VGint paramType);
--
getParameterf :: VGHandle -> VGenum -> IO VGfloat
getParameterf = vgGetParameterf

-- | Get the 'VGint' value of the parameter on the supplied 
-- handle.
--
-- 'getParameteri' corresponds to the OpenVG function 
-- @vgGetParameteri@.
--
-- > VGint vgGetParameteri (VGHandle object, VGint paramType);
--
getParameteri :: VGHandle -> VGenum -> IO VGint
getParameteri = vgGetParameteri

-- | Get the maximum number of values that the respective 
-- 'getParameterfv' or 'getParameteriv' call on the supplied 
-- handle would return for the given 'ParamType'.
--
-- 'getParameterVectorSize' corresponds to the OpenVG function 
-- @vgGetParameterVectorSize@.
--
-- > VGint vgGetParameterVectorSize (VGHandle object, VGint paramType);
-- 
getParameterVectorSize :: VGHandle -> VGenum -> IO VGint
getParameterVectorSize = vgGetParameterVectorSize

-- | Get the list of 'VGfloat' values for the parameter on the 
-- supplied handle.
--
-- 'getParameterfv' corresponds to the OpenVG function 
-- @vgGetParameterfv@.
--
-- > void vgGetParameterfv(VGHandle object, 
-- >                       VGint paramType, 
-- >                       VGint count, VGfloat * values)
--
getParameterfv :: VGHandle -> VGenum -> VGint -> IO [VGfloat]
getParameterfv h typ i = 
    vgGetParameterfv h typ i >>= peekArray (fromIntegral i)


-- | Get the list of 'VGint' values for the parameter on the 
-- supplied handle.
--
-- 'getParameteriv' corresponds to the OpenVG function 
-- @vgGetParameteriv@.
--
-- > void vgGetParameteriv(VGHandle object, 
-- >                       VGint paramType, 
-- >                       VGint count, VGint * values)
--
getParameteriv :: VGHandle -> VGenum -> VGint -> IO [VGint]
getParameteriv h typ i =
    vgGetParameteriv h typ i >>= peekArray (fromIntegral i)
                
--------------------------------------------------------------------------------    
    
marshalParamType :: ParamType -> VGenum
marshalParamType x = case x of
    MatrixMode                -> vg_MATRIX_MODE 
    FillRule                  -> vg_FILL_RULE
    ImageQuality              -> vg_IMAGE_QUALITY
    RenderingQuality          -> vg_RENDERING_QUALITY
    BlendMode                 -> vg_BLEND_MODE
    ImageMode                 -> vg_IMAGE_MODE
    ScissorRects              -> vg_SCISSOR_RECTS
    StrokeLineWidth           -> vg_STROKE_LINE_WIDTH
    StrokeCapStyle            -> vg_STROKE_CAP_STYLE
    StrokeJoinStyle           -> vg_STROKE_JOIN_STYLE
    StrokeMiterLimit          -> vg_STROKE_MITER_LIMIT
    StrokeDashPattern         -> vg_STROKE_DASH_PATTERN
    StrokeDashPhase           -> vg_STROKE_DASH_PHASE 
    StrokeDashPhaseReset      -> vg_STROKE_DASH_PHASE_RESET
    TileFillColor             -> vg_TILE_FILL_COLOR 
    ClearColor                -> vg_CLEAR_COLOR 
    -- ParamGlyphOrigin       -> vg_GLYPH_ORIGIN         {- Not in shiva-vg -}
    Masking                   -> vg_MASKING
    Scissoring                -> vg_SCISSORING 
    PixelLayout               -> vg_PIXEL_LAYOUT
    ScreenLayout              -> vg_SCREEN_LAYOUT 
    FilterFormatLinear        -> vg_FILTER_FORMAT_LINEAR
    FilterFormatPremultiplied -> vg_FILTER_FORMAT_PREMULTIPLIED
    FilterChannelMask         -> vg_FILTER_CHANNEL_MASK 
    MaxScissorRects           -> vg_MAX_SCISSOR_RECTS 
    MaxDashCount              -> vg_MAX_DASH_COUNT
    MaxKernelSize             -> vg_MAX_KERNEL_SIZE
    MaxSaparableKernelSize    -> vg_MAX_SEPARABLE_KERNEL_SIZE
    MaxColorRampStops         -> vg_MAX_COLOR_RAMP_STOPS
    MaxImageWidth             -> vg_MAX_IMAGE_WIDTH
    MaxImageHeight            -> vg_MAX_IMAGE_HEIGHT
    MaxImagePixels            -> vg_MAX_IMAGE_PIXELS
    MaxImageBytes             -> vg_MAX_IMAGE_BYTES
    MaxFloat                  -> vg_MAX_FLOAT
    MaxGaussianStdDeviation   -> vg_MAX_GAUSSIAN_STD_DEVIATION 

--------------------------------------------------------------------------------



