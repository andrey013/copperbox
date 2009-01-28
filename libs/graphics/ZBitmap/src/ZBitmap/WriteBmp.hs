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

module ZBitmap.WriteBmp where

import ZBitmap.Datatypes
import ZBitmap.Utils ( paddingMeasure )

import Data.Array.IArray ( (!), bounds )
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char ( ord ) 
import Data.Word 

import System.IO


type Output = BS.ByteString

type BMPout = Output -> Output


writeBmp :: FilePath -> BMPfile -> IO ()
writeBmp path bmp = let bmpstream = putBmpFile bmp $ BS.empty in do
    h <- openBinaryFile path WriteMode
    BS.hPut h bmpstream
    hClose h  

putBmpFile :: BMPfile -> BMPout
putBmpFile (BMPfile hdr dib o_pspec body) = 
    putBMPheader hdr . putV3Dibheader dib . putBody body
   


putBMPheader :: BMPheader -> BMPout
putBMPheader (BMPheader sz off)  = 
    outChar 'B' . outChar 'M' . outW32le sz 
                . outW16le 0  . outW16le 0   . outW32le off


putV3Dibheader :: V3Dibheader -> BMPout
putV3Dibheader dib = 
    outW32le 40 . outW32le (_bmp_width dib) 
                . outW32le (_bmp_height dib) 
                . outW16le 1                      -- 1 colour plane
                . outW16le (marshalBmpBitsPerPixel $ _bits_per_pixel dib)
                . outW32le 0                      -- compression
                . outW32le (_data_size dib)
                . outW32le 0                      -- horizontal res
                . outW32le 0                      -- vertical res
                . outW32le 0                      -- colours in palette
                . outW32le 0                      -- all colours important


putBody :: DibImageData -> BMPout
putBody = outByteString

          
putBodyArr :: ImageData' -> BMPout
putBodyArr arr = step [0..height] where
    step []     = id
    step (y:ys) = putRGBLine y width arr . step ys
    (_,(width,height)) = bounds arr

-- +1 when getting the padding measure because the array width is
-- the count from zero of the arraysize. 
putRGBLine :: Word32 -> Word32 -> ImageData' -> BMPout
putRGBLine row width arr = line [0..width]. padW (paddingMeasure $ width+1) 
  where
    line []     = id
    line (x:xs) = putRGBcolour (arr!(x,row)) . line xs
  


putRGBcolour :: RGBcolour -> BMPout
putRGBcolour (RGBcolour r g b) = out3 r g b

padW :: Word32 -> BMPout
padW i = step $ i `mod` 4
  where
    step 0 = id
    step 1 = out1 0
    step 2 = out2 0 0 
    step 3 = out3 0 0 0 
    step _ = error "erk - unreachable padW"
    
    

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

out3 :: Word8 -> Word8 -> Word8 -> (BS.ByteString -> BS.ByteString)
out3 a b c = (BS.cons a) . (BS.cons b) . (BS.cons c)

out4 :: Word8 -> Word8 -> Word8 -> Word8 -> (BS.ByteString -> BS.ByteString)
out4 a b c d = (BS.cons a) . (BS.cons b) . (BS.cons c) . (BS.cons d)


