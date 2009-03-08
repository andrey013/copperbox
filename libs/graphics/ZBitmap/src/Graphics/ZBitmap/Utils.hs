{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.Utils
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

module Graphics.ZBitmap.Utils where

import Graphics.ZBitmap.InternalSyntax

import Data.Array.IArray



import Prelude hiding ( (^) )
import qualified Prelude as Pre

-- Avoid warnings due to Integral Integer defaulting 
infixr 8 ^
(^) :: Integral a => a -> a -> a
(^) = (Pre.^)


paletteSize :: BmpBitsPerPixel -> Int 
paletteSize B1_Monochrome     = 2 ^ 1
paletteSize B4_Colour16       = 2 ^ 4     
paletteSize B8_Colour256      = 2 ^ 8     
paletteSize B16_HighColour    = 0   
paletteSize B24_TrueColour    = 0
paletteSize B32_TrueColour    = 0

rowsColumns :: BmpBitmap -> (Int,Int)
rowsColumns (BmpBitmap _ hdr _ _) = 
  (fromIntegral $ bmp_height hdr, fromIntegral $ bmp_width hdr)



cstyle2Darray :: IArray a e => Int -> Int -> [e] -> a (Int,Int) e 
cstyle2Darray w h xs = listArray ((0,0),(h-1,w-1)) xs

cstyle2DindexList :: Int -> Int -> [(Int,Int)]
cstyle2DindexList w h = [(y,x) | y <- [0..(h-1)], x <- [0..(w-1)] ]

arrayWidthHeight :: IArray a e => a (Int,Int) e -> (Int,Int)
arrayWidthHeight arr = 
    let ((c0,r0),(c1,r1)) = bounds arr in (1+r1-r0,1+c1-c0)
    
    
--------------------------------------------------------------------------------
-- 
{-

surfaceWidth :: BmpBitsPerPixel -> PixelCount -> PixelCount
surfaceWidth bpp w  = z * (((n * w) + 7) `div` 8) 
  where
    n = fromIntegral $ marshalBmpBitsPerPixel bpp
    -- effectively /z/ gets the ceiling of the division
    z = let z' = n `div` 8 in if z' == 0 then 1 else z' 
    

physicalWidth :: BmpBitsPerPixel -> PixelCount -> ByteCount
physicalWidth bpp w = 4 * (((n * w) + 31) `div` 32) 
  where 
    n = fromIntegral $ marshalBmpBitsPerPixel bpp

physicalSize :: BmpBitsPerPixel -> PixelCount ->  PixelCount -> ByteCount
physicalSize bpp w h = h * physicalWidth bpp w

-}

{-
--------------------------------------------------------------------------------
-- Indexes for traversal

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
-}    
        
-- demo01 = fold_lrdownM (\idx _ -> putStrLn $ show idx) 3 4 () 
-- demo02 = fold_rldownM (\idx _ -> putStrLn $ show idx) 3 4 ()  
-- demo03 = fold_lrupM   (\idx _ -> putStrLn $ show idx) 3 4 ()
-- demo04 = fold_rlupM   (\idx _ -> putStrLn $ show idx) 3 4 () 


--------------------------------------------------------------------------------
-- Colour conversion

data YCbCrColour = YCbCrColour 
      { ycbcr_y      :: Float
      , ycbcr_cb     :: Float
      , ycbcr_cr     :: Float
      }  
  deriving ( Eq, Show ) 
  
  
rgbToYCbCr :: RgbColour -> YCbCrColour
rgbToYCbCr (rv,gv,bv) = YCbCrColour y cb cr
  where
  y   =   (0.299  * r) + (0.587  * g) + (0.114  * b)
  cb  = (- 0.1687 * r) - (0.3313 * g) + (0.5    * b) + 128
  cr  =   (0.5    * r) - (0.4187 * g) - (0.0813 * b) + 128
  
  (r,g,b) = (fromIntegral rv, fromIntegral gv, fromIntegral bv)
  
  
yCbCrToRgb :: YCbCrColour -> RgbColour
yCbCrToRgb (YCbCrColour y cb cr) = (rv,gv,bv)
  where
  rv = round (y + 1.402   * (cr-128.0))
  gv = round (y - 0.34414 * (cb-128.0) - 0.71414 * (cr-128.0))
  bv = round (y + 1.772   * (cb-128.0))



{-

printCBBoxA :: IArray arr e => arr BitmapIndex e -> IO ()
printCBBoxA arr = printCBBox top_left bottom_right where
  (top_left,bottom_right) = bounds arr


-}

       