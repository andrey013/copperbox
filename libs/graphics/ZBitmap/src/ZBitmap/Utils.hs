{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.Utils
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

module ZBitmap.Utils where

import ZBitmap.Datatypes

import Data.Array.IArray
import Data.Array.Unboxed ( UArray )
import Data.Foldable ( foldlM )
import Data.List ( foldl' )
import Data.Word

import Text.PrettyPrint.HughesPJ

import Prelude hiding ( (^) )
import qualified Prelude as Pre

-- Avoid warnings due to Integral Integer defaulting 
infixr 8 ^
(^) :: Integral a => a -> a -> a
(^) = (Pre.^)


paletteSize :: BmpBitsPerPixel -> Int 
paletteSize B1_Monochrome       = 2 ^ 1
paletteSize B4_Colour16         = 2 ^ 4      
paletteSize B8_Colour256        = 2 ^ 8     
paletteSize B16_HighColour      = 0   
paletteSize B24_TrueColour24    = 0
paletteSize B32_TrueColour32    = 0

rowsColumns :: BmpBitmap -> (Word32,Word32)
rowsColumns bmp = (heightBmp bmp, widthBmp bmp)


--------------------------------------------------------------------------------
-- 

surfaceWidth :: BmpBitsPerPixel -> PixelCount -> PixelCount
surfaceWidth bpp w  = z * (((n * w) + 7) `div` 8) 
  where
    n = fromIntegral $ marshalBmpBitsPerPixel bpp
    -- effectively /z/ gets the ceiling of the division
    z = let z' = n `div` 8 in if z' == 0 then 1 else z' 
    

physicalWidth :: BmpBitsPerPixel -> PixelCount ->  ByteCount
physicalWidth bpp w = 4 * (((n * w) + 31) `div` 32) 
  where 
    n = fromIntegral $ marshalBmpBitsPerPixel bpp

physicalSize :: BmpBitsPerPixel -> PixelCount ->  PixelCount -> ByteCount
physicalSize bpp w h = h * physicalWidth bpp w


listArrayFrom0 :: IArray UArray a => [a] -> UArray Int a
listArrayFrom0 xs = listArray (0,fromIntegral $ length xs - 1) xs


countdown :: Integral a => a -> [a]
countdown (n + 1) = enumFromThenTo n (n-1) 0
countdown _       = []

countup :: Integral a => a -> [a]  
countup (n + 1) = [0..n]
countup _       = []

fold_lr :: Integral idx => (idx -> b -> b) -> idx -> b -> b
fold_lr f cols a = 
    foldl' (flip f) a [c | c <- countup cols ]

fold_rl :: Integral idx => (idx -> b -> b) -> idx -> b -> b
fold_rl f cols a = 
    foldl' (flip f) a [c | c <- countdown cols ]
    

fold_lrdown :: Integral idx => ((idx,idx) -> b -> b) -> idx -> idx -> b -> b
fold_lrdown f rows cols a = 
    foldl' (flip f) a [(r,c) | r <- countup rows, c <- countup cols ]

fold_lrdownM :: (Integral idx, Monad m) => 
    ((idx,idx) -> b -> m b) -> idx -> idx -> b -> m b
fold_lrdownM f rows cols a = 
    foldlM (flip f) a [(r,c) | r <- countup rows, c <- countup cols ]        


fold_rldown :: Integral idx => ((idx,idx) -> b -> b) -> idx -> idx -> b -> b
fold_rldown f rows cols a = 
    foldl' (flip f) a [(r,c) | r <- countup rows, c <- countdown cols ]

fold_rldownM :: (Integral idx, Monad m) => 
    ((idx,idx) -> b -> m b) -> idx -> idx -> b -> m b
fold_rldownM f rows cols a = 
    foldlM (flip f) a [(r,c) | r <- countup rows, c <- countdown cols]  


fold_lrup :: Integral idx => ((idx,idx) -> b -> b) -> idx -> idx -> b -> b
fold_lrup f rows cols a = 
    foldl' (flip f) a [(r,c) | r <- countdown rows, c <- countup cols ]

fold_lrupM :: (Integral idx, Monad m) => 
    ((idx,idx) -> b -> m b) -> idx -> idx -> b -> m b
fold_lrupM f rows cols a = 
    foldlM (flip f) a [(r,c) | r <- countdown rows, c <- countup cols ]  
    

fold_rlup :: Integral idx => ((idx,idx) -> b -> b) -> idx -> idx -> b -> b
fold_rlup f rows cols a = 
    foldl' (flip f) a [(r,c) | r <- countdown rows, c <- countdown cols ]

fold_rlupM :: (Integral idx, Monad m) => 
    ((idx,idx) -> b -> m b) -> idx -> idx -> b -> m b
fold_rlupM f rows cols a = 
    foldlM (flip f) a [(r,c) | r <- countdown rows, c <- countdown cols ] 
    
        
-- demo01 = fold_lrdownM (\idx _ -> putStrLn $ show idx) 3 4 () 
-- demo02 = fold_rldownM (\idx _ -> putStrLn $ show idx) 3 4 ()  
-- demo03 = fold_lrupM   (\idx _ -> putStrLn $ show idx) 3 4 ()
-- demo04 = fold_rlupM   (\idx _ -> putStrLn $ show idx) 3 4 () 


   

rgbToYCbCr :: RgbColour -> YCbCrColour
rgbToYCbCr (RgbColour rv gv bv) = YCbCrColour y cb cr
  where
  y   =   (0.299  * r) + (0.587  * g) + (0.114  * b)
  cb  = (- 0.1687 * r) - (0.3313 * g) + (0.5    * b) + 128
  cr  =   (0.5    * r) - (0.4187 * g) - (0.0813 * b) + 128
  
  (r,g,b) = (fromIntegral rv, fromIntegral gv, fromIntegral bv)
  
  
yCbCrToRgb :: YCbCrColour -> RgbColour
yCbCrToRgb (YCbCrColour y cb cr) = RgbColour r g b
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
           
           
    
printCBBox :: TwoDIndex -> TwoDIndex -> IO ()
printCBBox (r0,c0) (r1,c1) = putStrLn $ render $ doc where
    doc =     (indent (cl-x) tl)   <+> text ".."   <+> (indent (cr-y) tr)
           $$ (indent (cl-3) dot)                   <> indent (8 + (cr-y)) dot
           $$ bl                   <+> text ".."   <+> br
    
    dot             = char '.'
    indent i d      = spaces i <> d
    spaces i        = text $ replicate i ' ' 

    (tl,x)          = mkCoord (r0,c0)
    (tr,y)          = mkCoord (r1,c0)
    (bl,cl)         = mkCoord (r0,c1)
    (br,cr)         = mkCoord (r1,c1)
    
    mkCoord :: (Word32,Word32) -> (Doc,Int)
    mkCoord (i,j)   = let s = show (i,j)  
                      in (text s, length s) 



printCBBoxA :: IArray arr e => arr TwoDIndex e -> IO ()
printCBBoxA arr = printCBBox top_left bottom_right where
  (top_left,bottom_right) = bounds arr


       