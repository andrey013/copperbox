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
import Graphics.ZBitmap.Utils

import Data.Array.IArray ( (!) )
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char ( ord )
import Data.List( foldl' ) 
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
putBmpFile (BmpBitmap hdr opal bdy) = 
    putBMPheader hdr . maybe id putBmpPalette opal 
                     . maybe id putBody bdy
   


putBMPheader :: BmpHeader -> BMPout
putBMPheader hdr  = 
    outChar 'B' . outChar 'M'  
                . outW32le (bmp_file_size       hdr) 
                . outReserved (reserved_data    hdr)
                . outW32le (image_data_offset   hdr)
                . putV3Dibheader (dib_header    hdr)
  where
    outReserved :: ReservedData -> BMPout
    outReserved (a,b) = outW16le a . outW16le b
    
     
putV3Dibheader :: BmpDibHeader -> BMPout
putV3Dibheader dib = 
    outDibSize dib . outW32le (bmp_width                 dib) 
                   . outW32le (bmp_height                dib) 
                   . outColourPlanes  dib
                   . outW16le (marshalBmpBitsPerPixel  $ bits_per_pixel    dib)
                   . outW32le (marshalBmpCompression   $ compression_type  dib) 
                   . outW32le (image_data_size           dib)
                   . outW32le (h_resolution              dib)
                   . outW32le (v_resolution              dib)
                   . outW32le (palette_depth             dib)
                   . outImptColours dib
    where
      outDibSize      = outW32le . literalLiteral . dib_size
      outColourPlanes = outW16le . literalLiteral . colour_planes
      outImptColours  = outW32le . literalLiteral . colours_used

putBmpPalette :: Palette -> BMPout 
putBmpPalette _ = error $ "putBmpPalette _TODO_" 

putBody :: BmpDibImageData -> BMPout
putBody arr   = foldl' fn id idxs where
    idxs      = uncurry cstyle2DindexList $ arrayWidthHeight arr
    fn f idx  = f . out1 (arr!idx)
   
    

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

{-
outByteString :: BS.ByteString -> (BS.ByteString -> BS.ByteString)
outByteString = BS.append
-}

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


