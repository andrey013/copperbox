{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.Asciitron
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

module ZBitmap.Asciitron where

import ZBitmap.Datatypes
import ZBitmap.Utils

import Data.Array.IArray  ( listArray, (!), IArray(..) )
import qualified Data.Array.MArray as MA
import Data.Array.ST      ( runSTUArray )
import Data.Array.Unboxed ( UArray )
import Data.List ( foldl' )
import Data.Word ( Word32 )

type AsciiPicture = UArray (Word32,Word32) Char 

showAsciiPicture :: AsciiPicture -> String
showAsciiPicture arr = 
    unlines $ snd $ fold_rlup fn (r1+1) (c1+1) ([],[]) 
  where
    
    fn (r,c) (xs,xss) | c == 0    = let a = arr!(r,c) in ([], (a:xs):xss)
                      | otherwise = let a = arr!(r,c) in ((a:xs),xss)  
    (_,(r1,c1)) = bounds arr
    


makeAsciiPicture :: Bitmap Word32 -> AsciiPicture
makeAsciiPicture bmp@(Bitmap w h _ _) =  runSTUArray $ do
    ascii <- MA.thaw uarr
    fold_lrdownM (f ascii) row_count col_count () 
    return ascii
  where
    row_count = h
    col_count = w   
    uarr      = blankAsciiPicture row_count col_count
    
    f ascii idx _ = let c = greyscale $ bmp `pixelAt` idx in
                    MA.writeArray ascii idx c
                    
                    
pixelAt :: Bitmap Word32 -> (Word32,Word32) -> RGBcolour                               
pixelAt (Bitmap _ _ fw a) (r,c) = 
    if (r>rmax || c>cmax) 
      then error $ "r=" ++ show r ++ ", c=" ++ show c 
                        ++ ", bounds= " ++ show (bounds a)
      else RGBcolour (a!idxR) (a!idxG) (a!idxB)
  where    
    plusX (y,x) i     = (y,x+i)
    ((r0,c0),(r1,c1)) = bounds a
    rmax = r1-r0
    cmax = c1-c0
    idxR = (r, c * 3)
    idxG = idxR `plusX` 1
    idxB = idxR `plusX` 2
    

                                
blankAsciiPicture :: Word32 -> Word32 -> AsciiPicture
blankAsciiPicture row_count col_count = 
    runSTUArray $ MA.newArray ((0,0),(row_count - 1,col_count - 1)) '/'






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

    
     

    