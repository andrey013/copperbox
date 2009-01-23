{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Colour manipulation
--
--------------------------------------------------------------------------------

module ZBmp1.Utils where

import ZBmp1.Datatypes

import Data.Array.IArray
import Data.Word

data YCbCrColour = YCbCrColour { 
      _y_val  :: Float,
      _cb     :: Float,
      _cr     :: Float
    }
  deriving ( Show )    

rgbToYCbCr :: RGBcolour -> YCbCrColour
rgbToYCbCr (RGBcolour rv gv bv) = YCbCrColour y cb cr
  where
  y   =   (0.299  * r) + (0.587  * g) + (0.114  * b)
  cb  = (- 0.1687 * r) - (0.3313 * g) + (0.5    * b) + 128
  cr  =   (0.5    * r) - (0.4187 * g) - (0.0813 * b) + 128
  
  (r,g,b) = (fromIntegral rv, fromIntegral gv, fromIntegral bv)
  
  
yCbCrToRgb :: YCbCrColour -> RGBcolour
yCbCrToRgb (YCbCrColour y cb cr) = RGBcolour r g b
  where
  r = round (y + 1.402   * (cr-128.0))
  g = round (y - 0.34414 * (cb-128.0) - 0.71414 * (cr-128.0))
  b = round (y + 1.772   * (cb-128.0))


row :: Int -> Array (Int,Int) a -> [a]
row y arr = foldr (\a xs -> arr!(a,y) : xs) [] [x..x'] 
  where
    ((x,_),(x',_)) = bounds arr 
    
column :: Int -> Array (Int,Int) a -> [a]
column x arr = foldr (\a xs -> arr!(x,a) : xs) [] [y..y'] 
  where
    ((_,y),(_,y')) = bounds arr
    

-- How much padding does a line need?
paddingMeasure :: Word32 -> Word32
paddingMeasure i = step $ (i*3) `mod` 4 where
    step n | n == 0     = 0
           | otherwise  = 4 - n

-- row width must be a multiple of 4. Answer is number of bits
paddedWidth :: Word32 -> Word32
paddedWidth w = w + pad (w `mod` 32) where
    pad n | n == 0     = 0
          | otherwise  = 32 - n

byteWidth :: Word32 -> Word32 
byteWidth w = step $ w `divMod` 32 where
    step (d,m) | m == 0    = 4 * d
               | otherwise = 4 * (1 + d) 



       