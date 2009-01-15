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
  -- * Creating and destroying paint objects 
  withPaint,
  createPaint, destroyPaint, 
  
  -- * Setting the current paint 
  setPaint, getPaint,
  
  -- * Setting paint parameters
  PaintType(..), 
  paintType,
  paintColor,
  colorRampSpreadMode,
  colorRampStops,
  colorRampPremultiplied,
  LinearGradient,
  linearGradient,
  RadialGradient, 
  radialGradient,
  tilingMode,
  
  maxStops,
  paintPattern,
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGfloat, VGImage, VGPaint, Point )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgCreatePaint, vgDestroyPaint, 
    vgSetPaint,
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
    geti, getParameteri, 
    setParameteri, setParameterfv ) 
    
import Graphics.Rendering.OpenVG.VG.Paths (
    PaintMode(..) )
import Graphics.Rendering.OpenVG.VG.Utils ( 
    Marshal(..), Unmarshal(..), enumValue, unmarshalIntegral, bitwiseOr )

import Graphics.Rendering.OpenGL.GL.StateVar (
    StateVar(), makeStateVar,
    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

--------------------------------------------------------------------------------
-- Creating and destroying paint objects 


-- | @withPaint@ - create a paint object, run an action on it, destroy the 
-- paint object.
withPaint :: (VGPaint -> IO a) -> IO a
withPaint action = do
    img   <- createPaint
    ans   <- action img
    destroyPaint img
    return ans
    
    
-- | @createPaint@ corresponds to the OpenVG function @vgCreatePaint@.
createPaint :: IO VGPaint  
createPaint = vgCreatePaint

-- | @destroyPaint@ corresponds to the OpenVG function @vgDestroyPaint@.
destroyPaint :: VGPaint -> IO ()
destroyPaint = vgDestroyPaint

--------------------------------------------------------------------------------
-- Setting the current paint 

-- | Set the paint mode on the supplied handle.
setPaint :: VGPaint -> SettableStateVar [PaintMode]
setPaint h = makeSettableStateVar $ \ms -> vgSetPaint h (bitwiseOr ms)
    

-- | Get the paint object currently set for the supplied paint mode. 
-- Note currently @vgGetPaint@ is not working and will throw a runtime error 
-- if called.
getPaint :: PaintMode -> GettableStateVar VGPaint
getPaint paint_mode = makeGettableStateVar $ vgGetPaint (marshal paint_mode)
  where
    vgGetPaint :: VGenum -> IO VGPaint     
    vgGetPaint = error "vgGetPaint - error"
    
--------------------------------------------------------------------------------
-- Setting paint parameters 

-- | PaintType determines the type of paint to be applied.
data PaintType = 
     Color
   | LinearGradient
   | RadialGradient
   | Pattern
   deriving ( Eq, Ord, Show )

-- Control the PaintType of the supplied handle, the default value is
-- @Color@.
paintType :: VGPaint -> StateVar PaintType
paintType handle = makeStateVar (getPaintType handle) (setPaintType handle) 
  where
    getPaintType :: VGPaint -> IO PaintType
    getPaintType h = do 
        a <- getParameteri h vg_PAINT_TYPE
        return $ unmarshalIntegral a 
    
    setPaintType :: VGPaint -> PaintType -> IO ()
    setPaintType h v = 
        setParameteri h vg_PAINT_TYPE (enumValue v)

        
-- Control the color of the supplied handle, the default value is
-- @red=0.0f, green=0.0f, blue=0.0f, alpha=1.0f@      
paintColor :: VGPaint -> SettableStateVar (Color4 VGfloat)
paintColor h = makeSettableStateVar $ 
    \(Color4 r g b a) -> setParameterfv h vg_PAINT_COLOR [r,g,b,a]

-- | Spread modes define how the color ramp stops are extended or
-- repeated to define interpolated color.
data ColorRampSpreadMode = 
     CPad
   | CRepeat
   | CReflect
   deriving ( Eq, Ord, Show )
   
-- | Control the @ColorRampSpreadMode@ of the supplied handle, 
-- the default value is @CPad@ aka VG_COLOR_RAMP_SPREAD_PAD. 
colorRampSpreadMode :: VGPaint -> SettableStateVar ColorRampSpreadMode
colorRampSpreadMode h = makeSettableStateVar $ 
    \a -> setParameteri h vg_PAINT_COLOR_RAMP_SPREAD_MODE (enumValue a)
                            
    
-- paintColorRampStops - is [VGfloat] good enough, or should it be 
-- [Color4 VGfloat]?  
    
-- | Control the color ramp stops of the supplied handle, 
-- the default value is @[]@. 
colorRampStops :: VGPaint -> SettableStateVar [VGfloat]
colorRampStops h = makeSettableStateVar $ 
    \xs -> setParameterfv h vg_PAINT_COLOR_RAMP_STOPS xs


-- | Control the color ramp stops of the supplied handle, 
-- the default value is @[]@. 
colorRampPremultiplied :: VGPaint -> SettableStateVar Bool
colorRampPremultiplied h = makeSettableStateVar $ 
    \a -> setParameteri h vg_PAINT_COLOR_RAMP_PREMULTIPLIED (enumValue a)

type LinearGradient = (Point,Point)

linearGradient :: VGPaint -> SettableStateVar LinearGradient
linearGradient h = makeSettableStateVar $ \((x0,y0),(x1,y1)) -> do
    setParameteri  h vg_PAINT_TYPE (fromIntegral vg_PAINT_LINEAR_GRADIENT)
    setParameterfv h vg_PAINT_LINEAR_GRADIENT [x0,y0,x1,y1]

type RadialGradient = (Point,Point,VGfloat)

radialGradient :: VGPaint -> SettableStateVar RadialGradient
radialGradient h = makeSettableStateVar $ \((x0,y0),(x1,y1),r) -> do
    setParameteri  h vg_PAINT_TYPE (fromIntegral vg_PAINT_RADIAL_GRADIENT)
    setParameterfv h vg_PAINT_RADIAL_GRADIENT [x0,y0,x1,y1,r]

data TilingMode = 
     TFill
   | TPad
   | TRepeat
   | TReflect
   deriving ( Eq, Ord, Show ) 
   
-- | Control the @TilingMode@ of the supplied handle, 
-- the default value is @TFill@ aka VG_TILE_FILL.
tilingMode :: VGPaint -> SettableStateVar TilingMode
tilingMode h = makeSettableStateVar $ 
    \a -> setParameteri h vg_PAINT_PATTERN_TILING_MODE (enumValue a)
    
    
-- | Get the maximum number of ramp stops supported by the implementation.
maxStops :: GettableStateVar VGint 
maxStops = makeGettableStateVar $ geti MaxColorRampStops


       
-- | @paintPattern@ - corresponds directly to the OpenVG call @vgPaintPattern@.
paintPattern :: VGPaint -> VGImage -> IO ()
paintPattern = vgPaintPattern


--------------------------------------------------------------------------------




marshalPaintType :: PaintType -> VGenum
marshalPaintType x = case x of
    Color -> vg_PAINT_TYPE_COLOR
    LinearGradient -> vg_PAINT_TYPE_LINEAR_GRADIENT
    RadialGradient -> vg_PAINT_TYPE_RADIAL_GRADIENT
    Pattern -> vg_PAINT_TYPE_PATTERN

instance Marshal PaintType where marshal = marshalPaintType


unmarshalPaintType :: VGenum -> PaintType
unmarshalPaintType x
    | x == vg_PAINT_TYPE_COLOR              = Color
    | x == vg_PAINT_TYPE_LINEAR_GRADIENT    = LinearGradient
    | x == vg_PAINT_TYPE_RADIAL_GRADIENT    = RadialGradient
    | x == vg_PAINT_TYPE_PATTERN            = Pattern
    | otherwise = error ("unmarshalPaintType: illegal value " ++ show x)

instance Unmarshal PaintType where unmarshal = unmarshalPaintType

marshalColorRampSpreadMode :: ColorRampSpreadMode -> VGenum
marshalColorRampSpreadMode x = case x of 
    CPad -> vg_COLOR_RAMP_SPREAD_PAD
    CRepeat -> vg_COLOR_RAMP_SPREAD_REPEAT
    CReflect -> vg_COLOR_RAMP_SPREAD_REFLECT

instance Marshal ColorRampSpreadMode where marshal = marshalColorRampSpreadMode

marshalTilingMode :: TilingMode -> VGenum
marshalTilingMode x = case x of
    TFill -> vg_TILE_FILL
    TPad -> vg_TILE_PAD
    TRepeat -> vg_TILE_REPEAT
    TReflect -> vg_TILE_REFLECT
   
instance Marshal TilingMode where marshal = marshalTilingMode

     