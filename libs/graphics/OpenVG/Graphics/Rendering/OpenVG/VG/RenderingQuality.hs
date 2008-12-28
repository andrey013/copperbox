{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.RenderingQuality
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 6 (Rendering Quality and Antialiasing) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.RenderingQuality  where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.Constants (
    vg_RENDERING_QUALITY_NONANTIALIASED, vg_RENDERING_QUALITY_FASTER,
    vg_RENDERING_QUALITY_BETTER,
    --
    vg_PIXEL_LAYOUT_UNKNOWN, 
    vg_PIXEL_LAYOUT_RGB_VERTICAL, vg_PIXEL_LAYOUT_BGR_VERTICAL,
    vg_PIXEL_LAYOUT_RGB_HORIZONTAL, vg_PIXEL_LAYOUT_BGR_HORIZONTAL,
    --
    vg_MATRIX_PATH_USER_TO_SURFACE, vg_MATRIX_IMAGE_USER_TO_SURFACE, 
    vg_MATRIX_FILL_PAINT_TO_USER, vg_MATRIX_STROKE_PAINT_TO_USER, 
    vg_MATRIX_GLYPH_USER_TO_SURFACE
    )  

data RenderingQuality = 
     RenderingQualityNonantialiased
   | RenderingQualityFaster
   | RenderingQualityBetter
   deriving ( Eq, Ord, Show )
      
data PixelLayout = 
     PixelLayoutUnknown
   | PixelLayoutRgbVertical
   | PixelLayoutBgrVertical
   | PixelLayoutRgbHorizontal
   | PixelLayoutBgrHorizontal
   deriving ( Eq, Ord, Show )
   
data MatrixMode =
     MatrixPathUserToSurface
   | MatrixImageUserToSurface
   | MatrixFillPaintToUser
   | MatrixStrokePaintToUser
   | MatrixGlyphUserToSurface
   deriving ( Eq, Ord, Show )   
   
marshalRenderingQuality :: RenderingQuality -> VGenum
marshalRenderingQuality x = case x of
    RenderingQualityNonantialiased -> vg_RENDERING_QUALITY_NONANTIALIASED  
    RenderingQualityFaster -> vg_RENDERING_QUALITY_FASTER
    RenderingQualityBetter -> vg_RENDERING_QUALITY_BETTER

marshalPixelLayout :: PixelLayout -> VGenum
marshalPixelLayout x = case x of
    PixelLayoutUnknown -> vg_PIXEL_LAYOUT_UNKNOWN
    PixelLayoutRgbVertical -> vg_PIXEL_LAYOUT_RGB_VERTICAL
    PixelLayoutBgrVertical -> vg_PIXEL_LAYOUT_BGR_VERTICAL
    PixelLayoutRgbHorizontal -> vg_PIXEL_LAYOUT_RGB_HORIZONTAL
    PixelLayoutBgrHorizontal -> vg_PIXEL_LAYOUT_BGR_HORIZONTAL
    

marshalMatrixMode :: MatrixMode -> VGenum
marshalMatrixMode x = case x of 
    MatrixPathUserToSurface -> vg_MATRIX_PATH_USER_TO_SURFACE
    MatrixImageUserToSurface -> vg_MATRIX_IMAGE_USER_TO_SURFACE
    MatrixFillPaintToUser -> vg_MATRIX_FILL_PAINT_TO_USER
    MatrixStrokePaintToUser -> vg_MATRIX_STROKE_PAINT_TO_USER
    MatrixGlyphUserToSurface -> vg_MATRIX_GLYPH_USER_TO_SURFACE
    
    