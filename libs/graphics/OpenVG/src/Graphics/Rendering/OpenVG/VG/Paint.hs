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
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Paint (
  createPaint, destroyPaint, setPaint,
  paintType, -- ...
  paintPattern
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGfloat, VGImage, VGPaint )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgCreatePaint, vgDestroyPaint, vgSetPaint,
    vgPaintPattern )
    
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_PAINT_TYPE, vg_PAINT_COLOR, vg_PAINT_COLOR_RAMP_SPREAD_MODE, 
    vg_PAINT_COLOR_RAMP_STOPS, vg_PAINT_COLOR_RAMP_PREMULTIPLIED, 
    vg_PAINT_LINEAR_GRADIENT, vg_PAINT_RADIAL_GRADIENT, 
    vg_PAINT_PATTERN_TILING_MODE,

    vg_PAINT_TYPE_COLOR, vg_PAINT_TYPE_LINEAR_GRADIENT, 
    vg_PAINT_TYPE_RADIAL_GRADIENT, vg_PAINT_TYPE_PATTERN,

    vg_COLOR_RAMP_SPREAD_PAD, vg_COLOR_RAMP_SPREAD_REPEAT, 
    vg_COLOR_RAMP_SPREAD_REFLECT,
            
    vg_TILE_FILL, vg_TILE_PAD, vg_TILE_REPEAT, vg_TILE_REFLECT )
import Graphics.Rendering.OpenVG.VG.Parameters ( 
    ParamType( MaxColorRampStops ),
    geti, getParameteri, setParameteri, setParameterfv ) 
    
import Graphics.Rendering.OpenVG.VG.Paths (
    PaintMode(..) )
import Graphics.Rendering.OpenVG.VG.Utils ( bitwiseOr )

import Graphics.Rendering.OpenGL.GL.StateVar (
    StateVar(), makeStateVar,
--    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar )
     
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
     Color
   | LinearGradient
   | RadialGradient
   | Pattern
   deriving ( Eq, Ord, Show )
   

   
data TilingMode = 
     TileFill
   | TilePad
   | TileRepeat
   | TileReflect
   deriving ( Eq, Ord, Show )   

createPaint :: IO VGPaint  
createPaint = vgCreatePaint

destroyPaint :: VGPaint -> IO ()
destroyPaint = vgDestroyPaint

setPaint :: VGPaint -> [PaintMode] -> IO ()
setPaint h ms = vgSetPaint h (bitwiseOr ms)

-- vgGetPaint 

-- | Getters and setters for Table 10.

paintType :: VGPaint -> StateVar PaintType
paintType ph = makeStateVar (getPaintType ph) (setPaintType ph) 
  where
    getPaintType :: VGPaint -> IO PaintType
    getPaintType h = do 
        a <- getParameteri h vg_PAINT_TYPE
        return $ unmarshalPaintType $ fromIntegral a 
    
    setPaintType :: VGPaint -> PaintType -> IO ()
    setPaintType h v = 
        setParameteri h vg_PAINT_TYPE (fromIntegral $ marshalPaintType v)
                    
{-
-- not implemented in shiva-vg                    
paintColor :: VGPaint -> StateVar VGuint
paintColor h = makeStateVar (vgGetColor h) (vgSetColor h) 
-}

                    
-- Gradient paint
linearGradientPaint :: VGPaint 
                    -> VGfloat -> VGfloat -> VGfloat -> VGfloat -> IO ()
linearGradientPaint h x0 y0 x1 y1 = do
    setParameteri  h vg_PAINT_TYPE (fromIntegral vg_PAINT_LINEAR_GRADIENT)
    setParameterfv h vg_PAINT_LINEAR_GRADIENT [x0,y0,x1,y1]



radialGradientPaint :: VGPaint 
                    -> VGfloat -> VGfloat -> VGfloat -> VGfloat -> VGfloat 
                    -> IO ()
radialGradientPaint h cx cy fx fy r = do
    setParameteri  h vg_PAINT_TYPE (fromIntegral vg_PAINT_RADIAL_GRADIENT)
    setParameterfv h vg_PAINT_RADIAL_GRADIENT [cx,cy,fx,fy,r]
    

maxStops :: GettableStateVar VGint 
maxStops = makeGettableStateVar $ geti MaxColorRampStops

data ColorRampSpreadMode = 
     CRSPad
   | CRSRepeat
   | CRSReflect
   deriving ( Eq, Ord, Show )
       

paintPattern :: VGPaint -> VGImage -> IO ()
paintPattern = vgPaintPattern


--------------------------------------------------------------------------------

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


marshalPaintType :: PaintType -> VGenum
marshalPaintType x = case x of
    Color -> vg_PAINT_TYPE_COLOR
    LinearGradient -> vg_PAINT_TYPE_LINEAR_GRADIENT
    RadialGradient -> vg_PAINT_TYPE_RADIAL_GRADIENT
    Pattern -> vg_PAINT_TYPE_PATTERN

unmarshalPaintType :: VGenum -> PaintType
unmarshalPaintType x
    | x == vg_PAINT_TYPE_COLOR              = Color
    | x == vg_PAINT_TYPE_LINEAR_GRADIENT    = LinearGradient
    | x == vg_PAINT_TYPE_RADIAL_GRADIENT    = RadialGradient
    | x == vg_PAINT_TYPE_PATTERN            = Pattern
    | otherwise = error ("unmarshalPaintType: illegal value " ++ show x)
    
    
marshalColorRampSpreadMode :: ColorRampSpreadMode -> VGenum
marshalColorRampSpreadMode x = case x of 
    CRSPad -> vg_COLOR_RAMP_SPREAD_PAD
    CRSRepeat -> vg_COLOR_RAMP_SPREAD_REPEAT
    CRSReflect -> vg_COLOR_RAMP_SPREAD_REFLECT

marshalTilingMode :: TilingMode -> VGenum
marshalTilingMode x = case x of
    TileFill -> vg_TILE_FILL
    TilePad -> vg_TILE_PAD
    TileRepeat -> vg_TILE_REPEAT
    TileReflect -> vg_TILE_REFLECT
   
       
     