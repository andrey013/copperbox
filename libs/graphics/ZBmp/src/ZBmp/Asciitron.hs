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
    

makeAsciiPicture :: Word32 -> Word32 -> ImageData' -> AsciiPicture
makeAsciiPicture row_count col_count arr =  runSTUArray $ do
    ascii <- MA.thaw uarr
    fold_lrdownM (f ascii) row_count col_count () 
    return ascii
  where 
    uarr      = blankAsciiPicture row_count col_count
    pixelAtE  = pixelAt col_count row_count 
    
    f ascii idx _ = let c = greyscale $ arr `pixelAtE` idx in
                    MA.writeArray ascii idx c

pixelAt :: Word32 -> Word32 -> ImageData' -> (Word32,Word32) -> RGBcolour                               
pixelAt w h a idx@(r,c) = 
    if(r>rmax || c>cmax) 
      then error $ "r=" ++ show r ++ ", c=" ++ show c 
                        ++ ", bounds= " ++ show (bounds a)
      else a!idx 
  where
    ((r0,c0),(r1,c1)) = bounds a
    rmax = r1-r0 ; cmax = c1-c0
                                
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

-- Really we ought to resample the image so we can be sure it will fit onscreen
quickAsciiHack :: ImageData' -> [String]
quickAsciiHack arr = foldl' (\ss y -> line y :ss) [] [0..h] 
  where
    line y = foldr (\x s -> (greyscale $ arr!(x,y)) : s) "" [0..w]    
    ((_,_),(w,h)) = bounds arr
    
     

    