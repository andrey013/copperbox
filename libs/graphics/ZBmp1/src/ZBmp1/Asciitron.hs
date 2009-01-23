{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp1.Asciitron
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

module ZBmp1.Asciitron where

import ZBmp1.Datatypes
import ZBmp1.Utils

import Data.Array.IArray ( listArray, (!) , elems)
import Data.Array.Unboxed ( UArray )
import Data.Bits
import Data.Word

import Numeric ( showHex )


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

quickAsciiHack' :: Word32 -> Word32 -> ImageData -> String
quickAsciiHack' w' h' arr = foldr f [] (elems arr)
  where f e a = (showHex e []) ++ (' ': a)


quickAsciiHack :: Word32 -> Word32 -> ImageData -> String
quickAsciiHack w' h' arr = step ((0,0,0), "") 
  where
    step :: ((Int,Int,Int), String) -> String
    
    step (idx@(r,c,ci), acc)
        | r == h          = {- reverse $ -} acc
        | otherwise       = let a = if testBit (arr!(r * width + c)) ci 
                                      then ' ' else '@'
                            in step $ next idx (a:acc) 

    width                 = fromIntegral $ byteWidth w'
    (w,h)                 = (fromIntegral w', fromIntegral h')        

    next :: (Int,Int,Int) -> String -> ((Int,Int,Int), String)
    next (r,c,ci) acc 
        | c * 8 + ci == w-1   = ((r+1,0,0), '\n':acc)
        | ci == 7             = ((r,c+1,0), acc)
        | otherwise           = ((r,c,ci+1), acc)
                    

    
     

    