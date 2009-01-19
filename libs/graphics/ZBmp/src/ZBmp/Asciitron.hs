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
import qualified Data.Foldable as F
import Data.Word

greyPalette :: UArray Int Char
greyPalette = listArray (0,length xs) xs where
    xs = " `.,'-~*:+!?&%#@"   -- 16 levels
    
    
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
quickAsciiHack arr = third $ F.foldr fn (1,"",[]) arr where
    fn a (x,s,ss) | x == w    = (1,   [greyscale a],   s:ss)
                  | otherwise = (x+1, greyscale a : s, ss)

    third (_,_,c) = c
    
    ((_,_),(w,_)) = bounds arr
    
     

    