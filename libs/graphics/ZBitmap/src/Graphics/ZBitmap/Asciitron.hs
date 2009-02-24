{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.Asciitron
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

module Graphics.ZBitmap.Asciitron (
  AsciiPicture,
  makeAsciiPicture,
  showAsciiPicture,
  
) where

import Graphics.ZBitmap.InternalSyntax
import Graphics.ZBitmap.Utils
import Graphics.ZBitmap.ZBitmap


import Data.Array.IArray  ( listArray, (!), IArray(..) )
import qualified Data.Array.MArray as MA
import Data.Array.ST      ( runSTUArray )
import Data.Array.Unboxed ( UArray )


type AsciiPicture = UArray (Int,Int) Char 


makeAsciiPicture :: ZBitmap -> AsciiPicture
makeAsciiPicture bmp@(ZBitmap w h _ _) =  runSTUArray $ do
    ascii <- MA.thaw uarr
    fold_lrdownM (f ascii) row_count col_count () 
    return ascii
  where
    row_count = h
    col_count = w   
    uarr      = blankAsciiPicture row_count col_count
    
    f ascii idx _ = let c = greyscale $ bmp `pixelAt` idx in
                    MA.writeArray ascii idx c
                    
                    
showAsciiPicture :: AsciiPicture -> String
showAsciiPicture arr = 
    unlines $ snd $ fold_rlup fn (r1+1) (c1+1) ([],[]) 
  where
    
    fn (r,c) (xs,xss) | c == 0    = let a = arr!(r,c) in ([], (a:xs):xss)
                      | otherwise = let a = arr!(r,c) in ((a:xs),xss)  
    (_,(r1,c1)) = bounds arr
    


                    
                    
pixelAt :: ZBitmap -> (Int,Int) -> PaletteColour                               
pixelAt (ZBitmap _ _ _ a) (r,col) = PaletteColour (a!(r,c)) (a!(r,c+1)) (a!(r,c+2))
  where c = col * 4
    

                                
blankAsciiPicture :: Int -> Int -> AsciiPicture
blankAsciiPicture rows cols = 
    listArray ((0,0),(rows - 1,cols - 1)) (replicate (rows*cols) ' ')





-- Black is (0,0,0) and white is (255,255,255) hence the darkest 
-- char '@' is indexed at zero.
-- Having 4 full white chars seems to get better pictures.
greyPalette :: UArray Int Char
greyPalette = listArray (0,length xs) xs where
    xs = "@$#%&!+-^,.`    "   -- 16 levels
    
    
colourPitch :: PaletteColour -> Int
colourPitch a = lim $ floor $ y / 16
  where
    (YCbCrColour y _ _) = rgbToYCbCr a
    
    lim i | i > 15    = 15
          | i < 0     = 0
          | otherwise = i

greyscale :: PaletteColour -> Char
greyscale = (greyPalette !) . colourPitch

    
     

    