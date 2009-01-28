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

module ZBitmap.Convert where

import ZBitmap.Datatypes
import ZBitmap.Utils ( bmpRowWidth, fold_lrdownM )


import qualified Data.Array.MArray as MA
import Data.Array.ST ( runSTArray, runSTUArray )
import qualified Data.ByteString as BS
import Data.Word


valid_palette_spec_sizes   :: [Word32] 
valid_palette_spec_sizes = map (4*) [2^1, 2^4, 2^8]

palette :: BmpBitsPerPixel -> PaletteSpec -> Palette
palette bpp bs = 
    maybe (fk bpp) (\(sz,blen) -> makePalette sz blen bs) $ 
              checkPaletteSize bpp bs 
  where
    fk B1_Monochrome = error $ msg ++ show (4*(2^1)) ++ " for a mono bitmap"
    fk B4_Colour16   = error $ msg ++ show (4*(2^4)) ++ " for a 16 colour bitmap" 
    fk B8_Colour256  = error $ msg ++ show (4*(2^8)) ++ " for a 256 colour bitmap"
    fk _             = error $ "No palette spec for this resolution."
    
    msg = "Error: palette - unrecognized palette size, data length is " 
          ++ show (BS.length bs) ++ "\nbut it should be " 


    


checkPaletteSize :: BmpBitsPerPixel -> BS.ByteString -> Maybe (Word32,Int)
checkPaletteSize bpp bs = fn bpp (BS.length bs) where
    fn B1_Monochrome sz | sz == 4*(2^1)   = Just (2^1, sz) 
    fn B4_Colour16   sz | sz == 4*(2^4)   = Just (2^4, sz)
    fn B8_Colour256  sz | sz == 4*(2^8)   = Just (2^8, sz)
    fn _             _                    = Nothing

    
makePalette :: Word32 -> Int -> BS.ByteString -> Palette
makePalette sz bs_len bs = (\p -> Palette sz p) $ runSTArray $ do
    pal <-  MA.newArray (0, fromIntegral sz - 1) (RGBcolour 0 0 0)
    step pal 0 0
    return pal
  where
    step pal i j | j >= bs_len    = return ()
                 | otherwise  = let b   = bs `BS.index` j
                                    g   = bs `BS.index` (j+1)
                                    r   = bs `BS.index` (j+2)
                                    elt = (RGBcolour r g b)
                                in do MA.writeArray pal i elt
                                      step pal (i+1) (j+4)
                                   
    
-- blue green red _unused_


dibToBitmap :: BMPfile -> Bitmap Word32
dibToBitmap (BMPfile _ dib _ body) = 
    (\bmp -> Bitmap w h bw bmp) $ translate24bit h (getByteCount bw) (getByteCount bw) body 
                                $ newSurface h (getByteCount bw) 
  where
    w     = _bmp_width dib
    h     = _bmp_height dib
    bpp   = _bits_per_pixel dib
    bw    = bmpRowWidth w bpp
    
translate24bit :: 
    Word32 -> Word32 -> Word32 -> DibImageData -> PixelSurface Word32 -> PixelSurface Word32
translate24bit row_count col_count full_width bs uarr = runSTUArray $ do
    bmp <- MA.thaw uarr
    fold_lrdownM (f bmp) row_count col_count () 
    return bmp
  where
    f bmp (r,c) _ = let row = row_count - 1 - r
                        a = bs `BS.index` (fromIntegral $ row * full_width + c) 
                    in MA.writeArray bmp (r,c) a
  
  
newSurface :: Word32 -> Word32 -> PixelSurface Word32
newSurface row_count col_count = 
    runSTUArray $ MA.newArray ((0,0),(row_count - 1,col_count - 1)) 0
    
       