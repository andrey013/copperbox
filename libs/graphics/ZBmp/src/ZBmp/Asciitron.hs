{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp.Asciitron
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Create an ascii representation of a bitmap.
--
--------------------------------------------------------------------------------

module ZBmp.Asciitron where

import ZBmp.Datatypes
import ZBmp.Utils

import Data.Array.IArray ( listArray, (!), IArray(..) )
import Data.Array.Unboxed ( UArray )
import Data.List ( foldl' )


greyPalette :: UArray Int Char
greyPalette = listArray (0,length xs) xs where
    xs = "@$#%&!+-^,.`    "   -- 16 levels
    
    
colourPitch :: RGBcolour -> Int
colourPitch a = lim $ floor $ y / 16
  where
    (YCbCrColour y _ _) = rgbToYCbCr a
    
    lim i | i > 15    = 15
          | i < 0     = 0
          | otherwise = i

greyscale :: RGBcolour -> Char
greyscale = (greyPalette !) . colourPitch

-- Really we ought to resample the image so we can be sure it wil fit onscreen
quickAsciiHack :: ImageData -> [String]
quickAsciiHack arr = foldl' (\ss y -> line y :ss) [] [0..h] 
  where
    line y = foldr (\x s -> (greyscale $ arr!(x,y)) : s) "" [0..w]    
    ((_,_),(w,h)) = bounds arr
    
     

    