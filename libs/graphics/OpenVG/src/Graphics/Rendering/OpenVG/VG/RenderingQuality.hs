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
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.RenderingQuality (
  RenderingQuality(..), renderingQuality,
  PixelLayout(..),
  MatrixMode(..), matrixMode, 
  pixelLayout,
  loadIdentity,
  loadMatrix,
  getMatrix,
  multMatrix,
  translate, 
  scale, 
  shear, 
  rotate
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGfloat, VGenum )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgLoadIdentity, vgLoadMatrix, vgGetMatrix, vgMultMatrix, 
    vgTranslate, vgScale, vgShear, vgRotate
    )
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_RENDERING_QUALITY_NONANTIALIASED, vg_RENDERING_QUALITY_FASTER,
    vg_RENDERING_QUALITY_BETTER,
    --
    vg_PIXEL_LAYOUT_UNKNOWN, 
    vg_PIXEL_LAYOUT_RGB_VERTICAL, vg_PIXEL_LAYOUT_BGR_VERTICAL,
    vg_PIXEL_LAYOUT_RGB_HORIZONTAL, vg_PIXEL_LAYOUT_BGR_HORIZONTAL,
    --
    vg_MATRIX_PATH_USER_TO_SURFACE, vg_MATRIX_IMAGE_USER_TO_SURFACE, 
    vg_MATRIX_FILL_PAINT_TO_USER, vg_MATRIX_STROKE_PAINT_TO_USER, 
    )  
import Graphics.Rendering.OpenVG.VG.Parameters ( 
    ParamType ( MatrixMode, RenderingQuality, ScreenLayout ), 
    seti, geti  )

import Graphics.Rendering.OpenGL.GL.StateVar (
    StateVar(), makeStateVar, SettableStateVar, makeSettableStateVar )   

import Foreign.Ptr ( Ptr )   
                   
data RenderingQuality = 
     Nonantialiased'
   | Faster'
   | Better'
   deriving ( Eq, Ord, Show )
      
data PixelLayout = 
     Unknown
   | RgbVertical
   | BgrVertical
   | RgbHorizontal
   | BgrHorizontal
   deriving ( Eq, Ord, Show )
   
data MatrixMode =
     PathUserToSurface
   | ImageUserToSurface
   | FillPaintToUser
   | StrokePaintToUser
   deriving ( Eq, Ord, Show )   

-- | Set the rendering quality - the default is /Better/.
renderingQuality :: SettableStateVar RenderingQuality  
renderingQuality = makeSettableStateVar $ \mode -> 
    seti RenderingQuality (fromIntegral $ marshalRenderingQuality mode) 

pixelLayout :: StateVar PixelLayout
pixelLayout = makeStateVar getPixelLayout setPixelLayout

getPixelLayout :: IO PixelLayout
getPixelLayout = do
    a <- geti ScreenLayout 
    return $ unmarshalPixelLayout $ fromIntegral a
    
setPixelLayout :: PixelLayout -> IO ()  
setPixelLayout a = 
    seti ScreenLayout (fromIntegral $ marshalPixelLayout a)    

-- | VGMatrix Mode    
matrixMode :: SettableStateVar MatrixMode  
matrixMode = makeSettableStateVar $ \mode -> 
    seti MatrixMode (fromIntegral $ marshalMatrixMode mode) 

loadIdentity :: IO ()
loadIdentity = vgLoadIdentity


loadMatrix :: Ptr VGfloat -> IO ()
loadMatrix = vgLoadMatrix

getMatrix :: IO (Ptr VGfloat)
getMatrix = vgGetMatrix

multMatrix :: Ptr VGfloat -> IO ()
multMatrix = vgMultMatrix


translate :: VGfloat -> VGfloat -> IO ()
translate = vgTranslate

scale :: VGfloat -> VGfloat -> IO ()
scale = vgScale
    
shear :: VGfloat -> VGfloat -> IO ()
shear = vgShear
    
rotate :: VGfloat -> IO ()
rotate = vgRotate    
    

--------------------------------------------------------------------------------
   
marshalRenderingQuality :: RenderingQuality -> VGenum
marshalRenderingQuality x = case x of
    Nonantialiased' -> vg_RENDERING_QUALITY_NONANTIALIASED  
    Faster' -> vg_RENDERING_QUALITY_FASTER
    Better' -> vg_RENDERING_QUALITY_BETTER

marshalPixelLayout :: PixelLayout -> VGenum
marshalPixelLayout x = case x of
    Unknown -> vg_PIXEL_LAYOUT_UNKNOWN
    RgbVertical -> vg_PIXEL_LAYOUT_RGB_VERTICAL
    BgrVertical -> vg_PIXEL_LAYOUT_BGR_VERTICAL
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
    PathUserToSurface -> vg_MATRIX_PATH_USER_TO_SURFACE
    ImageUserToSurface -> vg_MATRIX_IMAGE_USER_TO_SURFACE
    FillPaintToUser -> vg_MATRIX_FILL_PAINT_TO_USER
    StrokePaintToUser -> vg_MATRIX_STROKE_PAINT_TO_USER
    
    