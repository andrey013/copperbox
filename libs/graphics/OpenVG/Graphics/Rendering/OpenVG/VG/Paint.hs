{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Paint
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 9 (Paint) 
-- of the OpenVG 1.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Paint  where
import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum )
import Graphics.Rendering.OpenVG.Constants (
    vg_PAINT_TYPE, vg_PAINT_COLOR, vg_PAINT_COLOR_RAMP_SPREAD_MODE, 
    vg_PAINT_COLOR_RAMP_STOPS, vg_PAINT_COLOR_RAMP_PREMULTIPLIED, 
    vg_PAINT_LINEAR_GRADIENT, vg_PAINT_RADIAL_GRADIENT, 
    vg_PAINT_PATTERN_TILING_MODE )
    
data PaintParamType = 
    --  Color paint parameters
     PaintType
   | PaintColor
   | PaintColorRampSpreadMode
   | PaintColorRampStops
   | PaintColorRampPremultiplied
     -- Linear gradient paint parameters 
   | PaintLinearGradient
     -- Radial gradient paint parameters
   | PaintRadialGradient
     -- Pattern paint parameters
   | PaintPatternTilingMode
   deriving ( Eq, Ord, Show )
   
data PaintType = 
     PaintTypeColor
   | PaintTypeLinearGradient
   | PaintTypeRadialGradient
   | PaintTypePattern
   deriving ( Eq, Ord, Show )
   
data ColorRampSpreadMode = 
     ColorRampSpreadPad
   | ColorRampSpreadRepeat
   | ColorRampSpreadReflect
   deriving ( Eq, Ord, Show )
   
data TilingMode = 
     TileFill
   | TilePad
   | TileRepeat
   | TileReflect
   deriving ( Eq, Ord, Show )   

marshalPaintParamType :: PaintParamType -> VGenum
marshalPaintParamType x = case x of 
    PaintType -> vg_PAINT_TYPE
    PaintColor -> vg_PAINT_COLOR
    PaintColorRampSpreadMode -> vg_PAINT_COLOR_RAMP_SPREAD_MODE
    PaintColorRampStops -> vg_PAINT_COLOR_RAMP_STOPS
    PaintColorRampPremultiplied -> vg_PAINT_COLOR_RAMP_PREMULTIPLIED
    PaintLinearGradient -> vg_PAINT_LINEAR_GRADIENT
    PaintRadialGradient -> vg_PAINT_RADIAL_GRADIENT
    PaintPatternTilingMode -> vg_PAINT_PATTERN_TILING_MODE
   
     