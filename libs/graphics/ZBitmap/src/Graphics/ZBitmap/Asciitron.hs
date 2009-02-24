{-# LANGUAGE FlexibleContexts           #-}
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
import Graphics.ZBitmap.Traverse
import Graphics.ZBitmap.Utils ( YCbCrColour(..), rgbToYCbCr )
import Graphics.ZBitmap.ZBitmap

import Control.Monad.ST
import Data.Array.IArray  ( listArray, (!) )
import qualified Data.Array.MArray as MA
import Data.Array.ST      ( STUArray, runSTUArray )
import Data.Array.Unboxed ( UArray )

type AsciiPicture = Cartesian UArray (Int,Int) Char 


makeAsciiPicture :: ZBitmap -> AsciiPicture
makeAsciiPicture bmp@(ZBitmap w h _ _) = Cartesian $ runSTUArray $ do
    ascii <- MA.thaw uarr
    forEachRowColM_ (Cartesian uarr) (f ascii)
    return ascii
  where
    row_count = h
    col_count = w   
    uarr      = getCartesian $ blankAsciiPicture row_count col_count
  
    f :: STUArray s (Int,Int) Char -> (Int,Int) -> ST s ()
    f ascii idx  = let c = greyscale $ bmp `pixelAt` idx in
                   MA.writeArray ascii idx c

pixelAt :: ZBitmap -> (Int,Int) -> PaletteColour                               
pixelAt (ZBitmap _ _ _ a) (r,col) = 
    PaletteColour (a!(r,c)) (a!(r,c+1)) (a!(r,c+2))
  where c = col * 4

blankAsciiPicture :: Int -> Int -> AsciiPicture
blankAsciiPicture rows cols = Cartesian $ 
    listArray ((0,0),(rows - 1,cols - 1)) (replicate (rows*cols) ' ')
    


-- Black is (0,0,0) and white is (255,255,255) hence the darkest 
-- char '@' is indexed at zero.
-- Having 4 full white chars seems to get better pictures.
greyPalette :: UArray Int Char
greyPalette = listArray (0,length xs) xs where
    xs = "@$#%&!+-^,.`    "   -- 16 levels
    
    


greyscale :: PaletteColour -> Char
greyscale = (greyPalette !) . colourPitch

colourPitch :: PaletteColour -> Int
colourPitch a = lim $ floor $ y / 16
  where
    (YCbCrColour y _ _) = rgbToYCbCr a
    
    lim i | i > 15    = 15
          | i < 0     = 0
          | otherwise = i  

--------------------------------------------------------------------------------
--
              
showAsciiPicture :: AsciiPicture -> String            
showAsciiPicture = ($ []) . showsAsciiPicture
                    
showsAsciiPicture :: AsciiPicture -> ShowS
showsAsciiPicture arr = 
    forEachRowCol arr (\idx@(_,c) f -> 
                              if (c==xmax) 
                                then f . showChar (a!idx) . showChar '\n' 
                                else f . showChar (a!idx))                              
                         id
  where
    a     = getCartesian arr
    xmax  = colMax arr 
         
    

     

    