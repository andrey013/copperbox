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

module Graphics.Rendering.OpenVG.VG.Images (
  imageQuality, 
  maxImageWidth, maxImageHeight, maxImagePixels, maxImageBytes,
  createImage, destroyImage, 
  imageFormat, imageWidth, imageHeight,
  clearImage,
  imageSubData,
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( 
    VGenum, VGint, VGImage, VGbitfield, 
    marshalBool )
import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
    vgCreateImage, vgDestroyImage, vgClearImage,
    vgImageSubData, vgGetImageSubData, 
    vgCopyImage )
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
    ParamType ( ParamImageQuality, 
                ParamMaxImageWidth, ParamMaxImageHeight, 
                ParamMaxImagePixels, ParamMaxImageBytes ) )

import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar,
   SettableStateVar, makeSettableStateVar )        

import Foreign.Ptr ( Ptr )


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
--   | FormatA1       (not supported in shiva-vg)
--   | FormatA4       (not supported in shiva-vg)
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


imageQuality :: SettableStateVar ImageQuality  
imageQuality = makeSettableStateVar $ \mode -> 
    seti ParamImageQuality (fromIntegral $ marshalImageQuality mode)      

maxImageWidth :: GettableStateVar VGint
maxImageWidth = makeGettableStateVar $
    geti ParamMaxImageWidth 

maxImageHeight :: GettableStateVar VGint
maxImageHeight = makeGettableStateVar $
    geti ParamMaxImageHeight
    
maxImagePixels :: GettableStateVar VGint
maxImagePixels = makeGettableStateVar $
    geti ParamMaxImagePixels

maxImageBytes :: GettableStateVar VGint
maxImageBytes = makeGettableStateVar $
    geti ParamMaxImageBytes
             
createImage :: ImageFormat -> VGint -> VGint -> [ImageQuality] -> IO VGImage 
createImage FormatsRGBA8888 width height qs = 
    vgCreateImage (marshalImageFormat FormatsRGBA8888) 
                  width 
                  height 
                  (bitwiseOrEnums marshalImageQuality qs) 
createImage _               _     _      _  = error $ "unsupported image format"



destroyImage :: VGImage -> IO () 
destroyImage = vgDestroyImage


imageFormat :: VGImage -> IO ImageFormat
imageFormat h = do 
    a <- getParameteri h vg_IMAGE_FORMAT
    return $ unmarshalImageFormat $ fromIntegral a 

imageWidth :: VGImage -> IO ImageFormat
imageWidth h = do 
    a <- getParameteri h vg_IMAGE_WIDTH
    return $ unmarshalImageFormat $ fromIntegral a 
    
imageHeight :: VGImage -> IO ImageFormat
imageHeight h = do 
    a <- getParameteri h vg_IMAGE_HEIGHT
    return $ unmarshalImageFormat $ fromIntegral a 


clearImage :: VGImage -> VGint -> VGint -> VGint -> VGint -> IO () 
clearImage = vgClearImage
            
-- TODO (Ptr? - the types of these functions needs more thought)
imageSubData :: VGImage -> Ptr a -> VGint -> ImageFormat
                  -> VGint -> VGint -> VGint -> VGint -> IO ()
imageSubData image imgdata stride fmt x y w h =
    vgImageSubData image imgdata stride (marshalImageFormat fmt) x y w h  
                   
getImageSubData :: VGImage -> Ptr a -> VGint -> ImageFormat
                    -> VGint -> VGint -> VGint -> VGint -> IO ()
getImageSubData image imgdata stride fmt x y w h =
    vgGetImageSubData image imgdata stride (marshalImageFormat fmt) x y w h 

-- childImage - not implemented in shiva-vg
-- getParent  - not implemented in shiva-vg

copyImage :: VGImage -> VGint -> VGint -> VGImage -> VGint -> VGint 
                -> VGint -> VGint -> Bool -> IO ()
copyImage dst dx dy src sx sy w h dither
    = vgCopyImage dst dx dy src sx sy w h (marshalBool dither)

--------------------------------------------------------------------------------

-- Utils

bitwiseOrEnums :: (a -> VGenum) -> [a] -> VGbitfield
bitwiseOrEnums f = sum . map (fromIntegral . f)
    
--------------------------------------------------------------------------------

marshalImageFormat :: ImageFormat -> VGenum
marshalImageFormat x = case x of 
    FormatsRGBX8888 -> vg_sRGBX_8888
    FormatsRGBA8888 -> vg_sRGBA_8888
    FormatsRGBA8888Pre -> vg_sRGBA_8888_PRE
    FormatsRGB565 -> vg_sRGB_565
    FormatsRGBA5551 -> vg_sRGBA_5551
    FormatsRGBA4444 -> vg_sRGBA_4444
    FormatsL8 -> vg_sL_8
    FormatlRGBX8888 -> vg_lRGBX_8888
    FormatlRGBA8888 -> vg_lRGBA_8888
    FormatlRGBA8888Pre -> vg_lRGBA_8888_PRE
    FormatlL8 -> vg_lL_8
    FormatA8 -> vg_A_8
    FormatBW1 -> vg_BW_1
    -- FormatA1 ->      (not supported in shiva-vg)
    -- FormatA4 ->      (not supported in shiva-vg)
    FormatsXRGB8888 -> vg_sXRGB_8888
    FormatsARGB8888 -> vg_sARGB_8888
    FormatsARGB8888Pre -> vg_sARGB_8888_PRE
    FormatsARGB1555 -> vg_sARGB_1555
    FormatsARGB4444 -> vg_sARGB_4444
    FormatlXRGB8888 -> vg_lXRGB_8888
    FormatlARGB8888 -> vg_lARGB_8888
    FormatlARGB8888Pre -> vg_lARGB_8888_PRE
    FormatsBGRX8888 -> vg_sBGRX_8888
    FormatsBGRA8888 -> vg_sBGRA_8888
    FormatsBGRA8888Pre -> vg_sBGRA_8888_PRE
    FormatsBGR565 -> vg_sBGR_565
    FormatsBGRA5551 -> vg_sBGRA_5551
    FormatsBGRA4444 -> vg_sBGRA_4444
    FormatlBGRX8888 -> vg_lBGRX_8888
    FormatlBGRA8888 -> vg_lBGRA_8888
    FormatlBGRA8888Pre -> vg_lBGRA_8888_PRE
    FormatsXBGR8888 -> vg_sXBGR_8888
    FormatsABGR8888 -> vg_sABGR_8888
    FormatsABGR8888Pre -> vg_sABGR_8888_PRE
    FormatsABGR1555 -> vg_sABGR_1555
    FormatsABGR4444 -> vg_sABGR_4444
    FormatlXBGR8888 -> vg_lXBGR_8888
    FormatlABGR8888 -> vg_lABGR_8888
    FormatlABGR8888Pre -> vg_lABGR_8888_PRE

unmarshalImageFormat :: VGenum -> ImageFormat
unmarshalImageFormat x
   | x == vg_sRGBX_8888 = FormatsRGBX8888
{-   
    FormatsRGBX8888 -> vg_sRGBX_8888
    FormatsRGBA8888 -> vg_sRGBA_8888
    FormatsRGBA8888Pre -> vg_sRGBA_8888_PRE
    FormatsRGB565 -> vg_sRGB_565
    FormatsRGBA5551 -> vg_sRGBA_5551
    FormatsRGBA4444 -> vg_sRGBA_4444
    FormatsL8 -> vg_sL_8
    FormatlRGBX8888 -> vg_lRGBX_8888
    FormatlRGBA8888 -> vg_lRGBA_8888
    FormatlRGBA8888Pre -> vg_lRGBA_8888_PRE
    FormatlL8 -> vg_lL_8
    FormatA8 -> vg_A_8
    FormatBW1 -> vg_BW_1
    -- FormatA1 ->      (not supported in shiva-vg)
    -- FormatA4 ->      (not supported in shiva-vg)
    FormatsXRGB8888 -> vg_sXRGB_8888
    FormatsARGB8888 -> vg_sARGB_8888
    FormatsARGB8888Pre -> vg_sARGB_8888_PRE
    FormatsARGB1555 -> vg_sARGB_1555
    FormatsARGB4444 -> vg_sARGB_4444
    FormatlXRGB8888 -> vg_lXRGB_8888
    FormatlARGB8888 -> vg_lARGB_8888
    FormatlARGB8888Pre -> vg_lARGB_8888_PRE
    FormatsBGRX8888 -> vg_sBGRX_8888
    FormatsBGRA8888 -> vg_sBGRA_8888
    FormatsBGRA8888Pre -> vg_sBGRA_8888_PRE
    FormatsBGR565 -> vg_sBGR_565
    FormatsBGRA5551 -> vg_sBGRA_5551
    FormatsBGRA4444 -> vg_sBGRA_4444
    FormatlBGRX8888 -> vg_lBGRX_8888
    FormatlBGRA8888 -> vg_lBGRA_8888
    FormatlBGRA8888Pre -> vg_lBGRA_8888_PRE
    FormatsXBGR8888 -> vg_sXBGR_8888
    FormatsABGR8888 -> vg_sABGR_8888
    FormatsABGR8888Pre -> vg_sABGR_8888_PRE
    FormatsABGR1555 -> vg_sABGR_1555
    FormatsABGR4444 -> vg_sABGR_4444
    FormatlXBGR8888 -> vg_lXBGR_8888
    FormatlABGR8888 -> vg_lABGR_8888
    FormatlABGR8888Pre -> vg_lABGR_8888_PRE
-}

   
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
