{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.Convert
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Convert between bitmap representations
--
--------------------------------------------------------------------------------

module ZBitmap.Convert (
  palette,
  dibToBitmap,
  
  monoTo24bit,
  
  
) where

import ZBitmap.Datatypes
import ZBitmap.Utils ( physicalWidth, fold_lrdownM )

import qualified Data.Array.MArray as MA
import Data.Array.ST ( runSTArray, runSTUArray )
import Data.Bits ( Bits(..) )
import qualified Data.ByteString as BS
import Data.Word

import Prelude hiding ( (^) )
import qualified Prelude as Pre

-- Avoid warnings due to Integral-Integer defaulting 
infixr 8 ^
(^) :: Integral a => a -> a -> a
(^) = (Pre.^)

palette :: BmpBitsPerPixel -> BmpPaletteSpec -> Palette
palette bpp bs = 
    maybe (fk bpp) (\(sz,blen) -> makePalette sz blen bs) $ 
              checkPaletteSize bpp bs 
  where
    fk B1_Monochrome = error $ msg ++ show (4*(2^1)::Int) ++ " for a mono bitmap"
    fk B4_Colour16   = error $ msg ++ show (4*(2^4)::Int) ++ " for a 16 colour bitmap" 
    fk B8_Colour256  = error $ msg ++ show (4*(2^8)::Int) ++ " for a 256 colour bitmap"
    fk _             = error $ "No palette spec for this resolution."
    
    msg = "Error: palette - unrecognized palette size, data length is " 
          ++ show (BS.length bs) ++ "\nbut it should be " 


checkPaletteSize :: BmpBitsPerPixel -> BS.ByteString -> Maybe (Word32,Int)
checkPaletteSize bpp bs = fn bpp (BS.length bs) where
    fn B1_Monochrome sz | sz == 4*(2^1)  = Just (2^1, sz) 
    fn B4_Colour16   sz | sz == 4*(2^4)  = Just (2^4, sz)
    fn B8_Colour256  sz | sz == 4*(2^8)  = Just (2^8, sz)
    fn _             _                    = Nothing

    
makePalette :: Word32 -> Int -> BS.ByteString -> Palette
makePalette sz bs_len bs = (\p -> Palette sz p) $ runSTArray $ do
    pal <-  MA.newArray (0, fromIntegral sz - 1) (RgbColour 0 0 0)
    step pal 0 0
    return pal
  where
    step pal i j | j >= bs_len    = return ()
                 | otherwise  = let b   = bs `BS.index` j
                                    g   = bs `BS.index` (j+1)
                                    r   = bs `BS.index` (j+2)
                                    elt = (RgbColour r g b)
                                in do MA.writeArray pal i elt
                                      step pal (i+1) (j+4)
                                       
-- blue green red _unused_


-- Bitmap is 32bit bitmap

dibToBitmap :: BmpBitmap -> Bitmap
dibToBitmap bm = 
    (\bmp -> Bitmap w h pw bmp) 
          $ translate24bit h pw pw (imageDataBmp bm) 
          $ newSurface h pw 
  where
    w     = widthBmp bm
    h     = heightBmp bm
    bpp   = bitsPerPixelBmp bm
    pw    = physicalWidth bpp w
    
translate24bit :: 
    Word32 -> Word32 -> Word32 -> BmpDibImageData -> PixelSurface -> PixelSurface
translate24bit row_count col_count full_width bs uarr = runSTUArray $ do
    bmp <- MA.thaw uarr
    fold_lrdownM (f bmp) row_count col_count () 
    return bmp
  where
    f bmp (r,c) _ = let row = row_count - 1 - r
                        a = bs `BS.index` (fromIntegral $ row * full_width + c) 
                    in MA.writeArray bmp (r,c) a
  

 
monoTo24bit :: BmpBitmap -> Bitmap
monoTo24bit bm = 
    (\bmp -> Bitmap w h pw bmp) 
          $ translateMono h w pw (imageDataBmp bm) 
          $ newSurface h (physicalWidth B32_TrueColour32 w) 
  where
    w     = widthBmp bm
    h     = heightBmp bm
    bpp   = bitsPerPixelBmp bm
    pw    = physicalWidth bpp w
    
translateMono :: 
    Word32 -> Word32 -> Word32 -> BmpDibImageData -> PixelSurface -> PixelSurface
translateMono row_count col_count full_width bs uarr = runSTUArray $ do
    bmp <- MA.thaw uarr
    fold_lrdownM (f bmp) row_count col_count () 
    return bmp
  where
    f bmp (r,c) _ = let row = row_count - 1 - r
                        (x,xi) = c `divMod` 8
                        w8 = bs `BS.index` (fromIntegral $ row * full_width + x)
                        a  = atMsb g w8 (fromIntegral xi) 
                    in do MA.writeArray bmp (r,c*4) a
                          MA.writeArray bmp (r,c*4+1) a
                          MA.writeArray bmp (r,c*4+2) a
    g True  = 255
    g False = 0
    
    
newSurface :: Word32 -> Word32 -> PixelSurface
newSurface row_count col_count = 
    runSTUArray $ MA.newArray ((0,0),(row_count - 1,col_count - 1)) 255

-- 0 msb - 7 lsb
atMsb :: (Bits a) => (Bool -> b) -> a -> Int -> b  
atMsb f a i = f $ a `testBit` j
  where j = (bitSize a) - 1 - i
  


{-
surfaceSizeAs24bit :: BmpBitmap -> SurfaceSize
surfaceSizeAs24bit b = fn (bitsPerPixelBmp b) (heightBmp b) (widthBmp b) where
    fn B1_Monochrome    r c  = undefined
    fn B4_Colour16      r c  = undefined
    fn B8_Colour256     r c  = undefined
    fn B16_HighColour   r c  = undefined
    fn B24_TrueColour24 r c  = (r,
    fn B32_TrueColour32 r c  = undefined
-}    
    

       