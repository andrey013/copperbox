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

import Data.Array.IArray ( Array, listArray, (!) ,  bounds)
import qualified Data.Array.MArray as MA
import Data.Array.ST ( runSTUArray )
import Data.Array.Unboxed ( UArray )
import Data.Bits
import Data.Word

-- import Numeric ( showHex )

type AsciiPicture = UArray (Word32,Word32) Char 

showAsciiPicture :: AsciiPicture -> String
showAsciiPicture arr = 
    unlines $ snd $ fold_rlup fn (r1+1) (c1+1) ([],[]) 
  where
    
    fn (r,c) (xs,xss) | c == 0    = let a = arr!(r,c) in ([], (a:xs):xss)
                      | otherwise = let a = arr!(r,c) in ((a:xs),xss)  
    (_,(r1,c1)) = bounds arr


makeAsciiPicture :: Word32 -> Word32 -> ImageData -> AsciiPicture
makeAsciiPicture row_count col_count arr =  runSTUArray $ do
    ascii <- MA.thaw uarr
    fold_lrdownM (f ascii) row_count col_count () 
    return ascii
  where 
    uarr      = blankAsciiPicture row_count col_count
    pixelAtE  = pixelAt col_count row_count 
    
    f ascii idx _ = let a = arr `pixelAtE` idx in
                    case a of True  -> MA.writeArray ascii idx ',' 
                              False -> MA.writeArray ascii idx '@' 
   
    
pixelAt :: Word32 -> Word32 -> ImageData -> TwoDIndex -> Bool 
pixelAt w h arr (r,c) = w8 `testBitMsb` (fromIntegral bit_idx)   
    where
      row                 = if (h==1) then 0 else h - 1 - r
      full_row_width      = paddedWidth w `div` 8
      (char_idx, bit_idx) = c `divMod` 8
      arr_idx             = let idx = row * full_row_width + char_idx in
                            if (idx > arrlen) 
                               then error $ "out-of-bounds " ++ show idx
                                            ++ ", row=" ++ show r
                                            ++ ", col=" ++ show c
                                            ++ ", frw=" ++ show full_row_width
                                            ++ ", char_idx=" ++ show char_idx
                                            ++ ", bit_idx=" ++ show bit_idx
                                            ++ "\n w=" ++ show w
                                            ++ ", h=" ++ show h
                                            
                               else idx
                               
      w8                  = arr ! arr_idx
      
      arrlen              = let (x0,x1) = bounds arr in x1-x0
      
       
blankAsciiPicture :: Word32 -> Word32 -> AsciiPicture
blankAsciiPicture row_count col_count = 
    runSTUArray $ MA.newArray ((0,0),(row_count - 1,col_count - 1)) '.'

  
sameDimensionsAscii :: Word32 -> Word32 -> ImageData -> String
sameDimensionsAscii w h img = 
    showAsciiPicture $ makeAsciiPicture w h img


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
quickAsciiHack' w' h' arr = step ((0,0,0), "") 
  where
    step :: ((Word32,Word32,Int), String) -> String
    
    step (idx@(r,c,ci), acc)
        | r == h          = (unlines . map reverse . lines) acc
        | otherwise       = let a = if testBit (arr!(r * width + c)) (7 - ci) 
                                      then ' ' else '@'
                            in step $ next idx (a:acc) 

    width                 = fromIntegral $ byteWidth w'
    (w,h)                 = (fromIntegral w', fromIntegral h')        

    next :: (Word32,Word32,Int) -> String -> ((Word32,Word32,Int), String)
    next (r,c,ci) acc 
        | c * 8 + (fromIntegral ci) == w-1  = ((r+1,0,0), '\n':acc)
        | ci == 7                           = ((r,c+1,0), acc)
        | otherwise                         = ((r,c,ci+1), acc)


                    

quickAsciiHack :: Word32 -> Word32 -> ImageData -> String
quickAsciiHack width height arr = 
    pixelAt 1 0 : pixelAt 1 1 : pixelAt 1 2 : []
  where 
    pixelAt m n = atMsb testBitf (msbWordAt m) n 
    
    msbWordAt m    = Msb $ arr ! m
    
    testBitf b = if b==True then '.' else '@' 

  
newtype Msb a = Msb { unMsb :: a }
        deriving (Eq, Ord, Read, Show, Bounded) 
        
-- 0 msb - 7 lsb
atMsb :: (Bits a) => (Bool -> b) -> Msb a -> Int -> b  
atMsb f (Msb a) i = f $ a `testBit` j
  where j = (bitSize a) - 1 - i
 
             
testBitMsb :: (Bits a) =>  a -> Int -> Bool  
testBitMsb a i = a `testBit` j
  where j = (bitSize a) - 1 - i          
        

    