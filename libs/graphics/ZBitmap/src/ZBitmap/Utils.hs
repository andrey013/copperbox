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


paletteSize :: BmpBitsPerPixel -> Int 
paletteSize B1_Monochrome       = 2 ^ 1
paletteSize B4_Colour16         = 2 ^ 4      
paletteSize B8_Colour256        = 2 ^ 8     
paletteSize B16_HighColour      = 0   
paletteSize B24_TrueColour24    = 0
paletteSize B32_TrueColour32    = 0

rowsColumns :: BMPfile -> (Word32,Word32)
rowsColumns (BMPfile _ dib _ _) = (_bmp_height dib, _bmp_width dib)

bmpRowWidth :: Word32 -> BmpBitsPerPixel -> ByteCount
bmpRowWidth w B1_Monochrome       = extendTo4ByteBoundary $ subdivide w 8    
bmpRowWidth w B4_Colour16         = extendTo4ByteBoundary $ subdivide w 2 
bmpRowWidth w B8_Colour256        = extendTo4ByteBoundary $ ByteCount w
bmpRowWidth w B16_HighColour      = extendTo4ByteBoundary $ ByteCount (w * 2)
bmpRowWidth w B24_TrueColour24    = extendTo4ByteBoundary $ ByteCount (w * 3)
bmpRowWidth w B32_TrueColour32    = ByteCount $ w * 4

subdivide w sub = let (d,m) = w `divMod` sub in 
                  if m==0 then ByteCount d else ByteCount $ d + 1

extendTo4ByteBoundary :: ByteCount -> ByteCount
extendTo4ByteBoundary (ByteCount w) = let (d,m) = w `divMod` 4 in
    if m == 0 then ByteCount w else ByteCount $ (d + 1) * 4

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


data YCbCrColour = YCbCrColour { 
      _y_val  :: Float,
      _cb     :: Float,
      _cr     :: Float
    }
  deriving ( Show )    

rgbToYCbCr :: RGBcolour -> YCbCrColour
rgbToYCbCr (RGBcolour rv gv bv) = YCbCrColour y cb cr
  where
  y   =   (0.299  * r) + (0.587  * g) + (0.114  * b)
  cb  = (- 0.1687 * r) - (0.3313 * g) + (0.5    * b) + 128
  cr  =   (0.5    * r) - (0.4187 * g) - (0.0813 * b) + 128
  
  (r,g,b) = (fromIntegral rv, fromIntegral gv, fromIntegral bv)
  
  
yCbCrToRgb :: YCbCrColour -> RGBcolour
yCbCrToRgb (YCbCrColour y cb cr) = RGBcolour r g b
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
    
    mkCoord :: Integral a => (a,a) -> (Doc,Int)
    mkCoord (i,j)   = let s = show (fromIntegral i, fromIntegral j) 
                      in (text s, length s) 

   
printCBBoxA :: IArray arr e => arr TwoDIndex e -> IO ()
printCBBoxA arr = printCBBox top_left bottom_right where
  (top_left,bottom_right) = bounds arr


       