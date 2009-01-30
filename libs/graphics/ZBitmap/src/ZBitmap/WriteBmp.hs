{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.WriteBmp
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

module ZBitmap.WriteBmp (
  writeBmp
) where

import ZBitmap.Datatypes


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
putBmpFile bmp = 
    putBMPheader bmp . putV3Dibheader bmp . putBody (imageDataBmp bmp)
   


putBMPheader :: BmpBitmap -> BMPout
putBMPheader bmp  = 
    outChar 'B' . outChar 'M'  
                . outW32le (fileSizeBmp bmp) 
                . outW16le r1  
                . outW16le r2   
                . outW32le (dataOffsetBmp bmp)
  where
    (r1,r2) = reservedBytesBmp bmp  

putV3Dibheader :: BmpBitmap -> BMPout
putV3Dibheader bmp = 
    outW32le 40 . outW32le (widthBmp bmp) 
                . outW32le (heightBmp bmp) 
                . outW16le (colourPlanesBmp bmp)  -- 1 colour plane
                . outW16le (marshalBmpBitsPerPixel $ bitsPerPixelBmp bmp)
                . outW32le (marshalBmpCompression $ compressionBmp bmp) 
                . outW32le (imageDataSizeBmp bmp)
                . outW32le (horizontalResolutionBmp bmp)
                . outW32le (verticalResolutionBmp bmp)
                . outW32le (paletteDepthBmp bmp)
                . outW32le (coloursUsedBmp bmp)


putBody :: BmpDibImageData -> BMPout
putBody = outByteString

   
    

--------------------------------------------------------------------------------
-- Output helpers


outW16le :: Word16 -> BMPout
outW16le i = out2 a b
  where 
  (a, r1)   = lowerEight i     
  (b, _)    = lowerEight r1

  
outW32le :: Word32 -> BMPout
outW32le i = out4 a b c d
  where 
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


