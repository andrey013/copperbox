{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Paint
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 9 (Paint) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Paint (
  -- * Creating and destroying paint objects 
  createPaint, destroyPaint, 
  withPaint,
  
  -- * Setting the current paint 
  setPaint, getPaint,
  
  -- * Setting paint parameters
  PaintType(..), 
  paintType,
  paintColor,
  maxColorRampStops,
  colorRampSpreadMode,
  ColorRampStops,
  colorRampStops,
  colorRampPremultiplied,

  LinearGradient,
  linearGradient,
  RadialGradient, 
  radialGradient,

  paintPattern,
  tilingMode
  
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGfloat, VGImage, VGPaint )
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
    
import Graphics.Rendering.OpenVG.VG.Paths ( PaintMode(..), marshalPaintMode )
import Graphics.Rendering.OpenVG.VG.Utils ( bitwiseOr, marshalBool )

import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

import Data.StateVar (
    StateVar(), makeStateVar,
    SettableStateVar, makeSettableStateVar,
    GettableStateVar, makeGettableStateVar )

import Data.Tensor ( Vector4(..) )

import Control.Monad ( liftM )

--------------------------------------------------------------------------------
-- Creating and destroying paint objects 

    
    
-- | Create a paint object and return a handle 
-- to it.
--
-- 'createPaint' corresponds to the OpenVG function 
-- @vgCreatePaint@.
--
createPaint :: IO VGPaint  
createPaint = vgCreatePaint

-- | Destroy a paint object and the resources associated with it.
--
-- 'destroyPaint' corresponds to the OpenVG function 
-- @vgDestoryPaint@.
--
destroyPaint :: VGPaint -> IO ()
destroyPaint = vgDestroyPaint

-- | Create a paint object, run an action on it, destroy the 
-- paint object.
--
-- 'withPaint' is a convenience function defined within the 
-- Haskell binding it does not have a corresponding OpenVG 
-- function.
--
withPaint :: (VGPaint -> IO a) -> IO a
withPaint action = do
    img   <- createPaint
    ans   <- action img
    destroyPaint img
    return ans


--------------------------------------------------------------------------------
-- Setting the current paint 

-- | Set the paint mode of the supplied paint handle.
--
-- 'setPaint' corresponds to the OpenVG function @vgSetPaint@
--
setPaint :: VGPaint -> SettableStateVar [PaintMode]
setPaint h = makeSettableStateVar $ vgSetPaint h . bitwiseOr marshalPaintMode
    

-- | Get the paint object currently set for the supplied paint 
-- mode.
-- 
-- 'getPaint' corresponds to the OpenVG function @vgGetPaint@.
--
-- \*\* Note -- currently @vgGetPaint@ is not implemented by 
-- ShivaVG, and this stub function will throw a runtime error 
-- when called. \*\*
--
getPaint :: PaintMode -> GettableStateVar VGPaint
getPaint = makeGettableStateVar . vgGetPaint . marshalPaintMode
  where
    vgGetPaint :: VGenum -> IO VGPaint     
    vgGetPaint = error "vgGetPaint - error"
    
--------------------------------------------------------------------------------
-- Setting paint parameters 

-- | PaintType determines the type of paint to be applied.
--
data PaintType = 
     Color
   | LinearGradient
   | RadialGradient
   | Pattern
   deriving ( Eq, Ord, Show )

-- | Query and set the 'PaintType' of the supplied handle.
-- 
-- 'paintType' is a read-write state variable corresponding to 
-- @VG_PAINT_TYPE@
--
-- The default value of 'paintType' is 'Color'.
--
paintType :: VGPaint -> StateVar PaintType
paintType handle = makeStateVar (getPaintType handle) (setPaintType handle) 
  where
    getPaintType :: VGPaint -> IO PaintType
    getPaintType h = liftM (unmarshalPaintType . fromIntegral) 
                           (getParameteri h vg_PAINT_TYPE )
    
    setPaintType :: VGPaint -> PaintType -> IO ()
    setPaintType h = 
        setParameteri h vg_PAINT_TYPE . fromIntegral . marshalPaintType


{-
-- ShivaVG does not seem to have implemented vgSetColor yet. 
setColor :: VGPaint -> VGuint -> IO ()
setColor = vgSetColor
-}

{-
-- ShivaVG does not seem to have implemented vgGetColor yet. 
getColor :: VGPaint -> IO VGuint
getColor = vgGetColor
-}


-- | Set the paint color of the supplied handle.
--
-- 'paintColor' is a write-only state variable corresponding to
-- @VG_PAINT_COLOR@.
--
-- The default value of 'paintColor' is black: 
-- @red=0.0f, green=0.0f, blue=0.0f, alpha=1.0f@.
--
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
   
-- | Set the color ramp spread mode of the supplied handle.
--
-- 'colorRampSpreadMode' is a write-only state variable 
-- corresponding to @VG_PAINT_COLOR_RAMP_SPREAD_MODE@.
-- 
-- The default value is @CPad@ aka VG_COLOR_RAMP_SPREAD_PAD. 
--
colorRampSpreadMode :: VGPaint -> SettableStateVar ColorRampSpreadMode
colorRampSpreadMode h = 
    let conv = fromIntegral . marshalColorRampSpreadMode in 
      makeSettableStateVar $
        setParameteri h vg_PAINT_COLOR_RAMP_SPREAD_MODE . conv                             
    

   
-- | Query the maximum number of ramp stops supported by the 
-- implementation.
--
-- 'maxColorRampStops' is a read-only state variable 
-- corresponding to @VG_MAX_COLOR_RAMP_STOPS@.
-- 
maxColorRampStops :: GettableStateVar VGint 
maxColorRampStops = makeGettableStateVar $ geti MaxColorRampStops


-- | ColorRampStops = (offset, color_value)
--
type ColorRampStops = (VGfloat, Vector4 VGfloat)

    
-- | Set the color ramp stops of the supplied handle.
--
-- 'colorRampStops' is a write-only state variable corresponding
-- to @VG_PAINT_COLOR_RAMP_STOPS@.
--
colorRampStops :: VGPaint -> SettableStateVar [ColorRampStops]
colorRampStops h = makeSettableStateVar $ 
    setParameterfv h vg_PAINT_COLOR_RAMP_STOPS . foldr fn []
  where
    fn (o,(Vector4 r g b a)) acc = o:r:g:b:a:acc 


-- | Set whether or not the color ramp stops are premultiplied.
--
-- 'colorRampPremultiplied' is a write-only state variable 
-- corresponding to @VG_PAINT_COLOR_RAMP_PREMULTIPLIED@.
--
colorRampPremultiplied :: VGPaint -> SettableStateVar Bool
colorRampPremultiplied h = let conv = fromIntegral . marshalBool in 
    makeSettableStateVar $ 
      setParameteri h vg_PAINT_COLOR_RAMP_PREMULTIPLIED . conv


-- | LinearGradient = (x0,y0,x1,y1). 
type LinearGradient = Vector4 VGfloat

-- | Set the linear gradient of the supplied handle.
--
-- 'linearGradient' is a write-only state variable, it corresponds
-- to a sequence of two OpenVG calls. The first sets the 
-- @VG_PAINT_TYPE@ parameter to @VG_PAINT_TYPE_LINEAR_GRADIENT@, 
-- the second sets the @VG_PAINT_LINEAR_GRADIENT@ parameter to the
-- supplied Vector4. 
-- 
linearGradient :: VGPaint -> SettableStateVar LinearGradient
linearGradient h = makeSettableStateVar $ \(Vector4 x0 y0 x1 y1) -> do
    setParameteri  h vg_PAINT_TYPE (fromIntegral vg_PAINT_LINEAR_GRADIENT)
    setParameterfv h vg_PAINT_LINEAR_GRADIENT [x0,y0,x1,y1]


-- | LinearGradient = ((cx,cy,fx,fy),r). 
-- 
-- @(cx,cy)@ is the centerpoint of the graient circle, @(fx,fy)@ 
-- is a focal point within the circle, @r@ is the radius.
type RadialGradient = (Vector4 VGfloat, VGfloat)

-- | Set the radial gradient of the supplied handle.
--
-- 'radialGradient' is a write-only state variable, it corresponds
-- to a sequence of two OpenVG calls. The first sets the 
-- @VG_PAINT_TYPE@ parameter to @VG_PAINT_TYPE_RADIAL_GRADIENT@, 
-- the  second sets the @VG_PAINT_RADIAL_GRADIENT@ parameter to 
-- the supplied 5 values. 
-- 
radialGradient :: VGPaint -> SettableStateVar RadialGradient
radialGradient h = makeSettableStateVar $ \((Vector4 x0 y0 x1 y1),r) -> do
    setParameteri  h vg_PAINT_TYPE (fromIntegral vg_PAINT_RADIAL_GRADIENT)
    setParameterfv h vg_PAINT_RADIAL_GRADIENT [x0,y0,x1,y1,r]


       
-- | Set the paint pattern on the on the paint object
-- with the new values from the supplied image
--
-- 'paintPattern' corresponds to the OpenVG function 
-- @vgPaintPattern@.
--
paintPattern :: VGPaint -> VGImage -> IO ()
paintPattern = vgPaintPattern


-- | Define colours for source pixels that lie outside the source
-- image.
--
data TilingMode = 
     TFill
   | TPad
   | TRepeat
   | TReflect
   deriving ( Eq, Ord, Show ) 
   
-- | Set the tiling mode of the supplied handle.
-- 
-- 'tilingMode' is a write-only state variable 
-- corresponding to @VG_PAINT_PATTERN_TILING_MODE@.
--
-- The default value is @TFill@ aka VG_TILE_FILL.
--
tilingMode :: VGPaint -> SettableStateVar TilingMode
tilingMode h = let conv = fromIntegral . marshalTilingMode in
    makeSettableStateVar $
      setParameteri h vg_PAINT_PATTERN_TILING_MODE . conv
    



--------------------------------------------------------------------------------




marshalPaintType :: PaintType -> VGenum
marshalPaintType x = case x of
    Color          -> vg_PAINT_TYPE_COLOR
    LinearGradient -> vg_PAINT_TYPE_LINEAR_GRADIENT
    RadialGradient -> vg_PAINT_TYPE_RADIAL_GRADIENT
    Pattern        -> vg_PAINT_TYPE_PATTERN


unmarshalPaintType :: VGenum -> PaintType
unmarshalPaintType x
    | x == vg_PAINT_TYPE_COLOR              = Color
    | x == vg_PAINT_TYPE_LINEAR_GRADIENT    = LinearGradient
    | x == vg_PAINT_TYPE_RADIAL_GRADIENT    = RadialGradient
    | x == vg_PAINT_TYPE_PATTERN            = Pattern
    | otherwise = error ("unmarshalPaintType: illegal value " ++ show x)

marshalColorRampSpreadMode :: ColorRampSpreadMode -> VGenum
marshalColorRampSpreadMode x = case x of 
    CPad     -> vg_COLOR_RAMP_SPREAD_PAD
    CRepeat  -> vg_COLOR_RAMP_SPREAD_REPEAT
    CReflect -> vg_COLOR_RAMP_SPREAD_REFLECT

marshalTilingMode :: TilingMode -> VGenum
marshalTilingMode x = case x of
    TFill    -> vg_TILE_FILL
    TPad     -> vg_TILE_PAD
    TRepeat  -> vg_TILE_REPEAT
    TReflect -> vg_TILE_REFLECT

