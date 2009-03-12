{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.BmpAdaptor.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utilities
--
--------------------------------------------------------------------------------

module Graphics.BmpAdaptor.Utils where

import Graphics.BmpAdaptor.InternalSyntax

import Data.Array.IArray
import Data.Bits
import Data.Word
import Numeric


paletteSize :: BmpBitsPerPixel -> Int 
paletteSize B1_Monochrome     = 2 ^ (1 :: Int)
paletteSize B4_Colour16       = 2 ^ (4 :: Int)     
paletteSize B8_Colour256      = 2 ^ (8 :: Int)    
paletteSize B16_HighColour    = 0   
paletteSize B24_TrueColour    = 0
paletteSize B32_TrueColour    = 0


cstyle2Darray :: IArray a e => Int -> Int -> [e] -> a (Int,Int) e 
cstyle2Darray w h xs = listArray ((0,0),(h-1,w-1)) xs

cstyle2DindexList :: Int -> Int -> [(Int,Int)]
cstyle2DindexList w h = [(y,x) | y <- [0..(h-1)], x <- [0..(w-1)] ]

cartesian2DindexList :: Int -> Int -> [(Int,Int)]
cartesian2DindexList w h = [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)]  ]


arrayWidthHeight :: IArray a e => a (Int,Int) e -> (Int,Int)
arrayWidthHeight arr = 
    let ((c0,r0),(c1,r1)) = bounds arr in (1+r1-r0,1+c1-c0)

arraySize :: IArray a e => a (Int,Int) e -> Int
arraySize = uncurry (*) . arrayWidthHeight

decodeRGB16bit :: Word8 -> Word8 -> RgbColour
decodeRGB16bit v1 v2 = (red,grn,blu) where
    red   =  bits7to3 v1
    grn   = (bits2to0 v1) + (bits7to6 v2)
    blu   =  bits5to1 v2 
    
    bits7to3  n = (`shiftR` 6) $ (n .&. 0x7c) 
    bits2to0  n = n .&. 0x07
    bits7to6  n = (`shiftR` 6) $ (n .&. 0xC0)  
    bits5to1  n = (`shiftR` 1) $ (n .&. 0x3E) 
          
showBin :: Integral a => a -> ShowS
showBin = showIntAtBase 2 (\i -> if i==0 then '0' else '1')

binRep :: Integral a => a -> String
binRep = ($ []) . showBin


-- sectionSizes ((header,dib_data), palette, pixel_data
 
sectionSizes :: BmpHeader -> ((Int,Int),Int,Int)
sectionSizes hdr = ((14,40),pal_size,pxl_size) 
  where
    pal_size = fromIntegral $ 4 * (paletteSize . bits_per_pixel . dib_header) hdr
    pxl_size = fromIntegral $ bmp_file_size hdr - image_data_offset hdr




calcBmpSize :: BmpBitsPerPixel -> Int -> Int
calcBmpSize bpp sz = 14 + 40 + paletteSize bpp + sz
    
--------------------------------------------------------------------------------
-- Colour conversion

data YCbCrColour = YCbCrColour 
      { ycbcr_y      :: Float
      , ycbcr_cb     :: Float
      , ycbcr_cr     :: Float
      }  
  deriving ( Eq, Show ) 
  
  
rgbToYCbCr :: RgbColour -> YCbCrColour
rgbToYCbCr (rv,gv,bv) = YCbCrColour y cb cr
  where
  y   =   (0.299  * r) + (0.587  * g) + (0.114  * b)
  cb  = (- 0.1687 * r) - (0.3313 * g) + (0.5    * b) + 128
  cr  =   (0.5    * r) - (0.4187 * g) - (0.0813 * b) + 128
  
  (r,g,b) = (fromIntegral rv, fromIntegral gv, fromIntegral bv)
  
  
yCbCrToRgb :: YCbCrColour -> RgbColour
yCbCrToRgb (YCbCrColour y cb cr) = (rv,gv,bv)
  where
  rv = round (y + 1.402   * (cr-128.0))
  gv = round (y - 0.34414 * (cb-128.0) - 0.71414 * (cr-128.0))
  bv = round (y + 1.772   * (cb-128.0))




       