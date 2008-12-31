{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Images
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 10 (Images) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Images where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.Constants (
    vg_IMAGE_QUALITY_NONANTIALIASED, vg_IMAGE_QUALITY_FASTER, 
    vg_IMAGE_QUALITY_BETTER, 
    vg_IMAGE_FORMAT, vg_IMAGE_WIDTH, vg_IMAGE_HEIGHT,
    vg_DRAW_IMAGE_NORMAL, vg_DRAW_IMAGE_MULTIPLY, vg_DRAW_IMAGE_STENCIL )
    


data ImageFormat = 
     -- RGB{A,X} channel ordering
     FormatsRGBX8888
   | FormatsRGBA8888
   | FormatsRGBA8888Pre
   | FormatsRGB565
   | FormatsRGBA5551
   | FormatsRGBA4444
   | FormatsL8
   | FormatlRGBX8888
   | FormatlRGBA8888
   | FormatlRGBA8888Pre
   | FormatlL8
   | FormatA8
   | FormatBW1
   | FormatA1
   | FormatA4
     -- {A,X}RGB channel ordering 
   | FormatsXRGB8888
   | FormatsARGB8888
   | FormatsARGB8888Pre
   | FormatsARGB1555
   | FormatsARGB4444
   | FormatlXRGB8888
   | FormatlARGB8888
   | FormatlARGB8888Pre
     -- BGR{A,X} channel ordering
   | FormatsBGRX8888
   | FormatsBGRA8888
   | FormatsBGRA8888Pre
   | FormatsBGR565
   | FormatsBGRA5551
   | FormatsBGRA4444
   | FormatlBGRX8888
   | FormatlBGRA8888
   | FormatlBGRA8888Pre
     -- {A,X}BGR channel ordering
   | FormatsXBGR8888
   | FormatsABGR8888
   | FormatsABGR8888Pre
   | FormatsABGR1555
   | FormatsABGR4444
   | FormatlXBGR8888
   | FormatlABGR8888
   | FormatlABGR8888Pre
   deriving ( Eq, Ord, Show )
   
data ImageQuality = 
     ImageQualityNonantialiased
   | ImageQualityFaster
   | ImageQualityBetter
   deriving ( Eq, Ord, Show )

data ImageParamType = 
     ImageParamFormat
   | ImageParamWidth
   | ImageParamHeight
   deriving ( Eq, Ord, Show )
   
data ImageMode = 
     DrawImageNormal
   | DrawImageMultiply
   | DrawImageStencil
   deriving ( Eq, Ord, Show )

marshalImageQuality :: ImageQuality -> VGenum
marshalImageQuality x = case x of
    ImageQualityNonantialiased -> vg_IMAGE_QUALITY_NONANTIALIASED
    ImageQualityFaster -> vg_IMAGE_QUALITY_FASTER
    ImageQualityBetter -> vg_IMAGE_QUALITY_BETTER
   
    
marshalImageParamType :: ImageParamType -> VGenum
marshalImageParamType x = case x of
    ImageParamFormat -> vg_IMAGE_FORMAT
    ImageParamWidth -> vg_IMAGE_WIDTH
    ImageParamHeight -> vg_IMAGE_HEIGHT

     
marshalImageMode :: ImageMode -> VGenum
marshalImageMode x = case x of
    DrawImageNormal -> vg_DRAW_IMAGE_NORMAL
    DrawImageMultiply -> vg_DRAW_IMAGE_MULTIPLY
    DrawImageStencil -> vg_DRAW_IMAGE_STENCIL
