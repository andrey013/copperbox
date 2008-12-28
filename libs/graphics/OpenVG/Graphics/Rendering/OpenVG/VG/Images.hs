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
   
data ImageMode = 
     DrawImageNormal
   | DrawImageMultiply
   | DrawImageStencil
   deriving ( Eq, Ord, Show )

   