{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Images
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module corresponds to section 10 (Images) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.Images (
  -- * Image quality
  ImageQuality(..), 
  imageQuality,
  
  -- * Image formats
  ImageFormat(..),
  
  -- * Creating and destroying images
  maxImageWidth, maxImageHeight, maxImagePixels, maxImageBytes,
  
  withImage,
  createImage, 
  destroyImage, 
  
  
  -- * Querying images
  imageFormat, imageWidth, imageHeight,
  
  -- * Reading and writing image pixels
  clearImage,
   
  imageSubData, getImageSubData,
  
  -- * Copying pixels between images
  copyImage, 
  
  -- * Drawing iamges to the drawing surface
  ImageMode(..),
  drawImageMode, 
  drawImage,
  
  -- * Reading and writing drawing surface pixels
  setPixels, writePixels, getPixels, readPixels, 
  
  -- * Copying portions of the drawing surface
  copyPixels,
  
     
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGImage, marshalBool )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgCreateImage, vgDestroyImage, vgClearImage,
    vgImageSubData, vgGetImageSubData, 
    vgCopyImage, vgDrawImage, 
    vgSetPixels, vgWritePixels, vgGetPixels, vgReadPixels, vgCopyPixels )
import Graphics.Rendering.OpenVG.VG.Constants (
    vg_sRGBX_8888, vg_sRGBA_8888, vg_sRGBA_8888_PRE, vg_sRGB_565,
    vg_sRGBA_5551, vg_sRGBA_4444, vg_sL_8, vg_lRGBX_8888,
    vg_lRGBA_8888, vg_lRGBA_8888_PRE, vg_lL_8, vg_A_8, vg_BW_1,

    vg_sXRGB_8888, vg_sARGB_8888, vg_sARGB_8888_PRE, vg_sARGB_1555,
    vg_sARGB_4444, vg_lXRGB_8888, vg_lARGB_8888, vg_lARGB_8888_PRE,
    vg_sBGRX_8888, vg_sBGRA_8888, vg_sBGRA_8888_PRE, vg_sBGR_565,
    vg_sBGRA_5551, vg_sBGRA_4444, vg_lBGRX_8888, vg_lBGRA_8888,
    vg_lBGRA_8888_PRE, vg_sXBGR_8888, vg_sABGR_8888,
    vg_sABGR_8888_PRE, vg_sABGR_1555, vg_sABGR_4444, vg_lXBGR_8888,
    vg_lABGR_8888, vg_lABGR_8888_PRE,
    
    vg_IMAGE_QUALITY_NONANTIALIASED, vg_IMAGE_QUALITY_FASTER, 
    vg_IMAGE_QUALITY_BETTER, 
    vg_IMAGE_FORMAT, vg_IMAGE_WIDTH, vg_IMAGE_HEIGHT,
    vg_DRAW_IMAGE_NORMAL, vg_DRAW_IMAGE_MULTIPLY, vg_DRAW_IMAGE_STENCIL )

import Graphics.Rendering.OpenVG.VG.Parameters ( 
    seti, geti, getParameteri, 
    ParamType ( ImageQuality, ImageMode,
                MaxImageWidth, MaxImageHeight, 
                MaxImagePixels, MaxImageBytes ) )

import Graphics.Rendering.OpenVG.VG.Utils ( bitwiseOr, unSizeM,     unSize )


import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size (..) )  

import Data.StateVar (
   GettableStateVar, makeGettableStateVar,
   SettableStateVar, makeSettableStateVar )        


import Foreign.Ptr ( Ptr )


--------------------------------------------------------------------------------
-- Image quality

-- | Resampling quality to be used when drawing images.
data ImageQuality = 
     Nonantialiased
   | Faster
   | Better
   deriving ( Eq, Ord, Show )
   
   
-- | Set the image quality - @imageQuality@ is typed wrapper
-- over this equivalent OpenVG code:
--
-- > vgSeti(VG_IMAGE_QUALITY, quality);
-- 
imageQuality :: SettableStateVar ImageQuality  
imageQuality = makeSettableStateVar $ 
    seti ImageQuality . fromIntegral . marshalImageQuality
    
    
--------------------------------------------------------------------------------
--  Image formats

-- | Image formats and colur spaces supported by OpenVG. 
--
-- \*\* NOTE - ImageFormat enumerates /all/ the image formats 
-- supported by OpenVG. ShivaVG currently only supports 
-- @sRGBA_8888@. \*\*
--
data ImageFormat = 
     -- RGB{A,X} channel ordering
     SRGBX8888
   | SRGBA8888
   | SRGBA8888Pre
   | SRGB565
   | SRGBA5551
   | SRGBA4444
   | SL8
   | LRGBX8888
   | LRGBA8888
   | LRGBA8888Pre
   | LL8
   | A8
   | BW1
     -- {A,X}RGB channel ordering 
   | SXRGB8888
   | SARGB8888
   | SARGB8888Pre
   | SARGB1555
   | SARGB4444
   | LXRGB8888
   | LARGB8888
   | LARGB8888Pre
     -- BGR{A,X} channel ordering
   | SBGRX8888
   | SBGRA8888
   | SBGRA8888Pre
   | SBGR565
   | SBGRA5551
   | SBGRA4444
   | LBGRX8888
   | LBGRA8888
   | LBGRA8888Pre
     -- {A,X}BGR channel ordering
   | SXBGR8888
   | SABGR8888
   | SABGR8888Pre
   | SABGR1555
   | SABGR4444
   | LXBGR8888
   | LABGR8888
   | LABGR8888Pre
   deriving ( Eq, Ord, Show )
  

    

--------------------------------------------------------------------------------
-- Creating and destroying images

-- | Get the maximum available width for the 'createImage' function.
-- @maxImageWidth@ is equivalent to this OpenVG code:
--
-- @ VGint imageMaxWidth = vgGeti(VG_MAX_IMAGE_WIDTH); @
maxImageWidth :: GettableStateVar VGint
maxImageWidth = makeGettableStateVar $ geti MaxImageWidth 

-- | Get the maximum available width for the 'createImage' function.
-- @maxImageWidth@ is equivalent to this OpenVG code:
--
-- @ VGint imageMaxWidth = vgGeti(VG_MAX_IMAGE_WIDTH); @
maxImageHeight :: GettableStateVar VGint
maxImageHeight = makeGettableStateVar $ geti MaxImageHeight

-- | Get the largest available value of the product of the @width@ and 
-- @height@ passed to the 'createImage' function.
-- @maxImagePixels@ is equivalent to this OpenVG code:
--
-- @ VGint imageMaxPixels = vgGeti(VG_MAX_IMAGE_PIXELS); @
maxImagePixels :: GettableStateVar VGint
maxImagePixels = makeGettableStateVar $ geti MaxImagePixels

-- | Get the largest available number of bytes that may make up the image data
-- passed to the 'createImage' function.
-- @maxImageBytes@ is equivalent to this OpenVG code:
--
-- @ VGint imageMaxBytes = vgGeti(VG_MAX_IMAGE_BYTES); @
maxImageBytes :: GettableStateVar VGint
maxImageBytes = makeGettableStateVar $ geti MaxImageBytes


-- | @withImage@ - create an image, run an action on it, destroy the image.
withImage :: ImageFormat -> Size -> [ImageQuality] -> (VGImage -> IO a) -> IO a
withImage fmt sz qual action = do
    img   <- createImage fmt sz qual
    ans   <- action img
    destroyImage img
    return ans
    
    
-- | @createImage@ corresponds to the OpenVG function @vgCreateImage@.             
createImage :: ImageFormat -> Size -> [ImageQuality] -> IO VGImage 
createImage SRGBA8888 sz qs = 
    unSizeM (\w h -> vgCreateImage (marshalImageFormat SRGBA8888)
                                   w 
                                   h 
                                   (bitwiseOr marshalImageQuality qs)) sz

createImage _         _  _  = error $ "unsupported image format"

-- | @destroyImage@ corresponds to the OpenVG function @vgDestroyImage@. 
destroyImage :: VGImage -> IO () 
destroyImage = vgDestroyImage


--------------------------------------------------------------------------------
-- Querying images
 
-- | Get the ImageFormat used to defined the image.
imageFormat :: VGImage -> GettableStateVar ImageFormat
imageFormat h = makeGettableStateVar $ 
   getParameteri h vg_IMAGE_FORMAT >>= 
   return . unmarshalImageFormat . fromIntegral

-- | Get the width used to defined the image.
imageWidth :: VGImage -> GettableStateVar VGint
imageWidth = makeGettableStateVar . flip getParameteri vg_IMAGE_WIDTH

-- | Get the height used to defined the image.
imageHeight :: VGImage -> GettableStateVar VGint
imageHeight = makeGettableStateVar . flip getParameteri vg_IMAGE_HEIGHT
 

--------------------------------------------------------------------------------
-- Reading and writing image pixels

-- | Fill the given rectangle inside the image with the current color setting
-- from the @StateVar@ 'clearColor'.
clearImage :: VGImage -> Position -> Size -> IO () 
clearImage handle (Position x y) = unSizeM $ vgClearImage handle x y
            
            
-- | @imageSubData@ corresponds to the OpenVG function @vgImageSubData@. 
imageSubData :: VGImage -> Ptr a -> VGint -> ImageFormat
                  -> Position -> Size -> IO ()
imageSubData image imgdata stride fmt (Position x y) =
    unSizeM $ vgImageSubData image imgdata stride (marshalImageFormat fmt) x y


-- | @getImageSubData@ corresponds to the OpenVG function 
-- @vgGetImageSubData@. 
--
getImageSubData :: VGImage -> Ptr a -> VGint -> ImageFormat
                    -> Position -> Size -> IO ()
getImageSubData image imgdata stride fmt (Position x y) = 
    unSizeM $ vgGetImageSubData image imgdata stride (marshalImageFormat fmt) x y
   
--------------------------------------------------------------------------------

-- childImage - not implemented in shiva-vg
-- getParent  - not implemented in shiva-vg

--------------------------------------------------------------------------------
-- Copying pixels between images

-- | @copyImage@ corresponds to the OpenVG function @vgCopyImage@.
copyImage :: VGImage -> Position -> VGImage -> Position
                -> Size -> Bool -> IO ()
copyImage dst (Position dx dy) src (Position sx sy) sz dither = 
    unSizeM (\w h -> vgCopyImage dst dx dy src sx sy w h (marshalBool dither)) sz

--------------------------------------------------------------------------------
-- Drawing iamges to the drawing surface

-- | Styles of image drawing   
data ImageMode = 
     Normal
   | Multiply
   | Stencil
   deriving ( Eq, Ord, Show )
   
-- | Set the image drawing mode - @drawImageMode@ is typed wrapper
-- over this equivalent OpenVG code:
--
-- @ VGImageMode drawImageMode; @
--
-- @ vgSeti(VG_IMAGE_MODE, drawImageMode); @
drawImageMode :: SettableStateVar ImageMode  
drawImageMode = makeSettableStateVar $
    seti ImageMode . fromIntegral . marshalImageMode

-- | @drawImage@ corresponds to the OpenVG function @vgDrawImage@. 
drawImage :: VGImage -> IO ()
drawImage = vgDrawImage


--------------------------------------------------------------------------------
-- Reading and writing drawing surface pixels

-- | @setPixels@ corresponds to the OpenVG function @vgSetPixels@. 
setPixels :: Position -> VGImage -> Position -> Size -> IO ()
setPixels (Position dx dy) src (Position sx sy) =
    unSizeM $ vgSetPixels dx dy src sx sy

-- | @writePixels@ corresponds to the OpenVG function @vgWritePixels@. 
writePixels :: Ptr a -> VGint -> ImageFormat -> Position -> Size -> IO ()
writePixels pixeldata stride fmt (Position dx dy) =  
    unSizeM $ vgWritePixels pixeldata stride (marshalImageFormat fmt) dx dy

-- | @getPixels@ corresponds to the OpenVG function @vgGetPixels@. 
getPixels :: VGImage  -> Position -> Position -> Size -> IO ()
getPixels dst (Position dx dy) (Position sx sy) =  
    unSizeM $ vgGetPixels dst dx dy sx sy

-- | @readPixels@ corresponds to the OpenVG function @vgReadPixels@.
readPixels :: Ptr a -> VGint -> ImageFormat -> Position -> Size -> IO ()
readPixels pixeldata stride fmt (Position sx sy) = unSizeM $
    vgReadPixels pixeldata stride (marshalImageFormat fmt) sx sy

--------------------------------------------------------------------------------
-- Copying portions of the drawing surface

-- | @copyPixels@ corresponds to the OpenVG function @vgCopyPixels@.
copyPixels :: Position -> Position -> Size -> IO ()
copyPixels (Position dx dy) (Position sx sy) = 
    unSizeM $ vgCopyPixels dx dy sx sy


                                  

    
--------------------------------------------------------------------------------

marshalImageFormat :: ImageFormat -> VGenum
marshalImageFormat x = case x of 
    SRGBX8888 -> vg_sRGBX_8888
    SRGBA8888 -> vg_sRGBA_8888
    SRGBA8888Pre -> vg_sRGBA_8888_PRE
    SRGB565 -> vg_sRGB_565
    SRGBA5551 -> vg_sRGBA_5551
    SRGBA4444 -> vg_sRGBA_4444
    SL8 -> vg_sL_8
    LRGBX8888 -> vg_lRGBX_8888
    LRGBA8888 -> vg_lRGBA_8888
    LRGBA8888Pre -> vg_lRGBA_8888_PRE
    LL8 -> vg_lL_8
    A8 -> vg_A_8
    BW1 -> vg_BW_1
    -- FormatA1 ->      (not supported in shiva-vg)
    -- FormatA4 ->      (not supported in shiva-vg)
    SXRGB8888 -> vg_sXRGB_8888
    SARGB8888 -> vg_sARGB_8888
    SARGB8888Pre -> vg_sARGB_8888_PRE
    SARGB1555 -> vg_sARGB_1555
    SARGB4444 -> vg_sARGB_4444
    LXRGB8888 -> vg_lXRGB_8888
    LARGB8888 -> vg_lARGB_8888
    LARGB8888Pre -> vg_lARGB_8888_PRE
    SBGRX8888 -> vg_sBGRX_8888
    SBGRA8888 -> vg_sBGRA_8888
    SBGRA8888Pre -> vg_sBGRA_8888_PRE
    SBGR565 -> vg_sBGR_565
    SBGRA5551 -> vg_sBGRA_5551
    SBGRA4444 -> vg_sBGRA_4444
    LBGRX8888 -> vg_lBGRX_8888
    LBGRA8888 -> vg_lBGRA_8888
    LBGRA8888Pre -> vg_lBGRA_8888_PRE
    SXBGR8888 -> vg_sXBGR_8888
    SABGR8888 -> vg_sABGR_8888
    SABGR8888Pre -> vg_sABGR_8888_PRE
    SABGR1555 -> vg_sABGR_1555
    SABGR4444 -> vg_sABGR_4444
    LXBGR8888 -> vg_lXBGR_8888
    LABGR8888 -> vg_lABGR_8888
    LABGR8888Pre -> vg_lABGR_8888_PRE


unmarshalImageFormat :: VGenum -> ImageFormat
unmarshalImageFormat x
    | x == vg_sRGBX_8888        = SRGBX8888
    | x == vg_sRGBA_8888        = SRGBA8888  
    | x == vg_sRGBA_8888_PRE    = SRGBA8888Pre
    | x == vg_sRGB_565          = SRGB565
    | x == vg_sRGBA_5551        = SRGBA5551
    | x == vg_sRGBA_4444        = SRGBA4444
    | x == vg_sL_8              = SL8
    | x == vg_lRGBX_8888        = LRGBX8888
    | x == vg_lRGBA_8888        = LRGBA8888
    | x == vg_lRGBA_8888_PRE    = LRGBA8888Pre
    | x == vg_lL_8              = LL8
    | x == vg_A_8               = A8
    | x == vg_BW_1              = BW1
     -- FormatA1  =       (not supported in shiva-vg)
     -- FormatA4  =       (not supported in shiva-vg)
    | x == vg_sXRGB_8888        = SXRGB8888
    | x == vg_sARGB_8888        = SARGB8888
    | x == vg_sARGB_8888_PRE    = SARGB8888Pre
    | x == vg_sARGB_1555        = SARGB1555
    | x == vg_sARGB_4444        = SARGB4444
    | x == vg_lXRGB_8888        = LXRGB8888
    | x == vg_lARGB_8888        = LARGB8888
    | x == vg_lARGB_8888_PRE    = LARGB8888Pre
    | x == vg_sBGRX_8888        = SBGRX8888
    | x == vg_sBGRA_8888        = SBGRA8888  
    | x == vg_sBGRA_8888_PRE    = SBGRA8888Pre  
    | x == vg_sBGR_565          = SBGR565 
    | x == vg_sBGRA_5551        = SBGRA5551  
    | x == vg_sBGRA_4444        = SBGRA4444  
    | x == vg_lBGRX_8888        = LBGRX8888  
    | x == vg_lBGRA_8888        = LBGRA8888  
    | x == vg_lBGRA_8888_PRE    = LBGRA8888Pre  
    | x == vg_sXBGR_8888        = SXBGR8888  
    | x == vg_sABGR_8888        = SABGR8888  
    | x == vg_sABGR_8888_PRE    = SABGR8888Pre  
    | x == vg_sABGR_1555        = SABGR1555  
    | x == vg_sABGR_4444        = SABGR4444  
    | x == vg_lXBGR_8888        = LXBGR8888  
    | x == vg_lABGR_8888        = LABGR8888  
    | x == vg_lABGR_8888_PRE    = LABGR8888Pre  
    | otherwise = error ("unmarshalImageFormat: illegal value " ++ show x)

   
marshalImageQuality :: ImageQuality -> VGenum
marshalImageQuality x = case x of
    Nonantialiased -> vg_IMAGE_QUALITY_NONANTIALIASED
    Faster -> vg_IMAGE_QUALITY_FASTER
    Better -> vg_IMAGE_QUALITY_BETTER


     
marshalImageMode :: ImageMode -> VGenum
marshalImageMode x = case x of
    Normal -> vg_DRAW_IMAGE_NORMAL
    Multiply -> vg_DRAW_IMAGE_MULTIPLY
    Stencil -> vg_DRAW_IMAGE_STENCIL
