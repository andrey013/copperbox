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

-- import Data.Array.IArray ( (!) )
import qualified Data.Array.MArray as MA
import Data.Array.ST ( runSTUArray )
import qualified Data.ByteString as BS
import Data.Word


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
    
       