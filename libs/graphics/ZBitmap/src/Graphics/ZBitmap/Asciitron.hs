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

import Graphics.ZBitmap.InternalBitmap 
import Graphics.ZBitmap.InternalSyntax
import Graphics.ZBitmap.Utils


import Control.Monad.ST

import Data.Array.IArray  ( IArray, bounds, listArray, (!) )
import qualified Data.Array.IArray as I 
import qualified Data.Array.MArray as MA
import Data.Array.ST      ( STUArray, runSTUArray )
import Data.Array.Unboxed ( UArray )

import Data.Foldable ( foldl', foldlM )


type AsciiPicture = UArray (Int,Int) Char 

makeAsciiPicture :: UniBitmap -> AsciiPicture
makeAsciiPicture (UniBitmap _ bmp) = runSTUArray $ do
    ascii <- MA.thaw uarr
    foldlM (f ascii) () $ cstyle2DindexList width height 
    return ascii
  where
    width     = imageWidth bmp
    height    = imageHeight bmp
    
    pixelAt   = colourAt bmp   
    uarr      = blankAsciiPicture width height
  
    f :: STUArray s (Int,Int) Char -> () -> (Int,Int) ->  ST s ()
    f ascii () idx = let c = greyscale $ pixelAt (convert idx) in
                     MA.writeArray ascii idx c
    -- bitmaps are stored upside down
    convert (r,c) = (height-1-r,c)
    
    
blankAsciiPicture :: Int -> Int -> AsciiPicture
blankAsciiPicture width height =  
    cstyle2Darray width height (replicate (width*height) ' ')
    


-- Black is (0,0,0) and white is (255,255,255) hence the darkest 
-- char '@' is indexed at zero.
-- Having 4 full white chars seems to get better pictures.
greyPalette :: UArray Int Char
greyPalette = listArray (0,length xs) xs where
    xs = "@$#%&!+-^,.`    "   -- 16 levels
    
    


greyscale :: RgbColour -> Char
greyscale = (greyPalette !) . colourPitch

colourPitch :: RgbColour -> Int
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
showsAsciiPicture arr = foldl' mkShowFun id (cstyle2DindexList w h)
  where
    (w,h) = arrayWidthHeight arr
    xmax  = w-1
    mkShowFun f (c,r)
      | (c,r) > snd (bounds arr) = error $ "showsAsciiPicture" ++ show (c,r) ++ show (bounds arr) 
      | r==xmax   = f . showChar (arr!(c,r)) . showChar '\n' 
      | otherwise = f . showChar (arr!(c,r))   
         
    

     

    