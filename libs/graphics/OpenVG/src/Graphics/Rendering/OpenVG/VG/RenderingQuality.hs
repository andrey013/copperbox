{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.RenderingQuality
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 6 (Rendering Quality and Antialiasing) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.RenderingQuality (
  
  -- * Rendering quality
  RenderingQuality(..), 
  renderingQuality,
  
  -- * Additional quality settings
  PixelLayout(..), 
  pixelLayout,
  
  -- * Matrix manipulation
  MatrixMode(..),
  matrixMode, 
  
  loadIdentity,
  loadMatrix,
  getMatrix,
  multMatrix,
  translate, 
  scale, 
  shear, 
  rotate
) where

import Graphics.Rendering.OpenVG.VG.Parameters

import Graphics.Rendering.OpenVG.Raw.VG.Core101 ( VGenum, VGfloat )
import Graphics.Rendering.OpenVG.Raw.VG.RenderingQuality

import Data.StateVar (
    StateVar(), makeStateVar, SettableStateVar, makeSettableStateVar )   

import Control.Applicative
import Foreign.Ptr ( Ptr )   


--------------------------------------------------------------------------------
-- Rendering quality
                     
-- | 'RenderingQuality' enumerates the settings available for
-- rendering quality. 
-- 
data RenderingQuality = 
     Nonantialiased'
   | Faster'
   | Better'
   deriving ( Eq, Ord, Show )

-- | Set the rendering quality.
--
-- 'renderingQuality' is a write-only state variable corresponding to
-- @VG_RENDERING_QUALITY@:
--
-- The default value for 'renderingQuality' is @Better\'@.
--
renderingQuality :: SettableStateVar RenderingQuality  
renderingQuality = makeSettableStateVar $
    seti RenderingQuality . fromIntegral . marshalRenderingQuality
    
--------------------------------------------------------------------------------
-- Additional quality settings 

-- | 'PixelLayout' enumerates the possible geometric layouts of 
-- color emissions within pixel elements.
--
data PixelLayout = 
     Unknown
   | RgbVertical
   | BgrVertical
   | RgbHorizontal
   | BgrHorizontal
   deriving ( Eq, Ord, Show )

-- | Set and query the pixel layout.
--
-- 'pixelLayout' is a read-write state variable corresponding to
-- @VG_PIXEL_LAYOUT@.
--
pixelLayout :: StateVar PixelLayout
pixelLayout = makeStateVar getPixelLayout setPixelLayout
  where
    getPixelLayout :: IO PixelLayout
    getPixelLayout = unmarshalPixelLayout . fromIntegral <$> geti ScreenLayout
        
    setPixelLayout :: PixelLayout -> IO ()  
    setPixelLayout = seti ScreenLayout . fromIntegral . marshalPixelLayout
    
--------------------------------------------------------------------------------
-- Matrix manipulation

-- | 'MatrixMode' enumerates the manipulable matrices.  
data MatrixMode =
     PathUserToSurface
   | ImageUserToSurface
   | FillPaintToUser
   | StrokePaintToUser
   deriving ( Eq, Ord, Show )   

-- | Set the matrix mode.
--
-- 'matrixMode' is a write-only state variable corresponding to
-- @VG_MATRIX_MODE@:
--
matrixMode :: SettableStateVar MatrixMode  
matrixMode = makeSettableStateVar $  
    seti MatrixMode . fromIntegral . marshalMatrixMode

-- | Set the current matrix to the identity matrix.
--
-- 'loadIdentity' corresponds to the OpenVG function 
-- @vgLoadIdentity@. 
--
loadIdentity :: IO ()
loadIdentity = vgLoadIdentity

-- | Set the current matrix to the supplied matrix.
--
-- 'loadMatrix' corresponds to the OpenVG function @vgLoadMatrix@.
-- 
-- \*\* Note - this function has an unfortunate type and should
-- be wrapped. \*\* 
--
loadMatrix :: Ptr VGfloat -> IO ()
loadMatrix = vgLoadMatrix

-- | Get the current matrix.
--
-- 'getMatrix' corresponds to the OpenVG function @vgGetMatrix@.
-- 
-- \*\* Note - this function has an unfortunate type and should
-- be wrapped. \*\* 
--
getMatrix :: IO (Ptr VGfloat)
getMatrix = vgGetMatrix

-- | Multiply the current matrix by the supplied matrix.
-- 
-- 'multMatrix' corresponds to the OpenVG function @vgMultMatrix@.
-- 
-- \*\* Note - this function has an unfortunate type and should
-- be wrapped. \*\* 
--
multMatrix :: Ptr VGfloat -> IO ()
multMatrix = vgMultMatrix

-- | Apply a translation to the current matrix.
--
-- 'translate' corresponds to the OpenVG function @vgTranslate@. 
--
translate :: VGfloat -> VGfloat -> IO ()
translate = vgTranslate

-- | Apply a scaling to the current matrix.
--
-- 'scale' corresponds to the OpenVG function @vgScale@. 
--
scale :: VGfloat -> VGfloat -> IO ()
scale = vgScale

-- | Apply a shear to the current matrix.
--
-- 'shear' corresponds to the OpenVG function @vgShear@.
--
shear :: VGfloat -> VGfloat -> IO ()
shear = vgShear

-- | Apply a rotation to the current matrix.
--
-- 'rotate' corresponds to the OpenVG function @vgRotate@.
--
rotate :: VGfloat -> IO ()
rotate = vgRotate    
    

--------------------------------------------------------------------------------
   
marshalRenderingQuality :: RenderingQuality -> VGenum
marshalRenderingQuality x = case x of
    Nonantialiased' -> vg_RENDERING_QUALITY_NONANTIALIASED  
    Faster'         -> vg_RENDERING_QUALITY_FASTER
    Better'         -> vg_RENDERING_QUALITY_BETTER


marshalPixelLayout :: PixelLayout -> VGenum
marshalPixelLayout x = case x of
    Unknown       -> vg_PIXEL_LAYOUT_UNKNOWN
    RgbVertical   -> vg_PIXEL_LAYOUT_RGB_VERTICAL
    BgrVertical   -> vg_PIXEL_LAYOUT_BGR_VERTICAL
    RgbHorizontal -> vg_PIXEL_LAYOUT_RGB_HORIZONTAL
    BgrHorizontal -> vg_PIXEL_LAYOUT_BGR_HORIZONTAL



unmarshalPixelLayout :: VGenum -> PixelLayout 
unmarshalPixelLayout x
    | x == vg_PIXEL_LAYOUT_UNKNOWN        = Unknown
    | x == vg_PIXEL_LAYOUT_RGB_VERTICAL   = RgbVertical 
    | x == vg_PIXEL_LAYOUT_BGR_VERTICAL   = BgrVertical 
    | x == vg_PIXEL_LAYOUT_RGB_HORIZONTAL = RgbHorizontal 
    | x == vg_PIXEL_LAYOUT_BGR_HORIZONTAL = BgrHorizontal 
    | otherwise = error ("unmarshalPixelLayout: illegal value " ++ show x)
    
    
marshalMatrixMode :: MatrixMode -> VGenum
marshalMatrixMode x = case x of 
    PathUserToSurface  -> vg_MATRIX_PATH_USER_TO_SURFACE
    ImageUserToSurface -> vg_MATRIX_IMAGE_USER_TO_SURFACE
    FillPaintToUser    -> vg_MATRIX_FILL_PAINT_TO_USER
    StrokePaintToUser  -> vg_MATRIX_STROKE_PAINT_TO_USER
    

