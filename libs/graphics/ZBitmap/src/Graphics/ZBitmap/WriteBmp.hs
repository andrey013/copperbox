{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.WriteBmp
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Write a bitmap to file.
--
--------------------------------------------------------------------------------

module Graphics.ZBitmap.WriteBmp (
  writeBmp
) where

import Graphics.ZBitmap.InternalSyntax


import Data.Bits
import qualified Data.ByteString as BS
import Data.Char ( ord ) 
import Data.Word 

import System.IO


type Output = BS.ByteString

type BMPout = Output -> Output


writeBmp :: FilePath -> BmpBitmap -> IO ()
writeBmp path bmp = let bmpstream = putBmpFile bmp $ BS.empty in do
    h <- openBinaryFile path WriteMode
    BS.hPut h bmpstream
    hClose h  

putBmpFile :: BmpBitmap -> BMPout
putBmpFile (BmpBitmap hdr dib _ bdy) = 
    putBMPheader hdr . putV3Dibheader dib . putBody bdy
   


putBMPheader :: BmpHeader -> BMPout
putBMPheader hdr  = 
    outChar 'B' . outChar 'M'  
                . outW32le (bmp_file_size       hdr) 
                . outW16le (reserved1           hdr)
                . outW16le (reserved2           hdr) 
                . outW32le (image_data_offset   hdr)


putV3Dibheader :: BmpDibHeader -> BMPout
putV3Dibheader dib = 
    outW32le 40 . outW32le (bmp_width                 dib) 
                . outW32le (bmp_height                dib) 
                . outW16le (colour_planes             dib)  -- 1 colour plane
                . outW16le (marshalBmpBitsPerPixel  $ bits_per_pixel    dib)
                . outW32le (marshalBmpCompression   $ compression_type  dib) 
                . outW32le (image_data_size           dib)
                . outW32le (h_resolution              dib)
                . outW32le (v_resolution              dib)
                . outW32le (palette_depth             dib)
                . outW32le (colours_used              dib)


putBody :: BmpDibImageData -> BMPout
putBody = outByteString

   
    

--------------------------------------------------------------------------------
-- Output helpers


outW16le :: Word16 -> BMPout
outW16le i = out2 a b where
    (a, r1)   = lowerEight i     
    (b, _)    = lowerEight r1

  
outW32le :: Word32 -> BMPout
outW32le i = out4 a b c d where
    (a, r1)   = lowerEight i     
    (b, r2)   = lowerEight r1
    (c, r3)   = lowerEight r2
    (d, _)    = lowerEight r3

outByteString :: BS.ByteString -> (BS.ByteString -> BS.ByteString)
outByteString = BS.append


outChar :: Char -> BMPout
outChar = out1 . fromIntegral . ord


lowerEight :: (Bits a, Integral a) => a -> (Word8, a)    
lowerEight n = (fromIntegral lower8, remain)
  where
    remain = n `shiftR` 8
    lower8 = n .&. 0xff 
    
out1 :: Word8 -> (BS.ByteString -> BS.ByteString)
out1 = BS.cons

out2 :: Word8 -> Word8 -> (BS.ByteString -> BS.ByteString)
out2 a b = (BS.cons a) . (BS.cons b) 

out4 :: Word8 -> Word8 -> Word8 -> Word8 -> (BS.ByteString -> BS.ByteString)
out4 a b c d = (BS.cons a) . (BS.cons b) . (BS.cons c) . (BS.cons d)


