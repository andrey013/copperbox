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

import Control.Monad ( (>=>) )
import Data.Array.IArray ( bounds, (!) )
import Data.Array.IO
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char ( ord )
import Data.List( foldl' ) 
import Data.Word 

import System.IO


type Output = BS.ByteString

type BMPout = Output -> Output


writeBmp :: FilePath -> BmpBitmap -> IO ()
writeBmp path (BmpBitmap hdr opal obdy) = maybe fk sk obdy where 
    fk        = error $ "no pixel data, no file written"
    sk bdy    = do h <- openBinaryFile path WriteMode
                   BS.hPut h bmpstream
                   arrayWrite h bdy
                   hClose h
    bmpstream = (putBMPheader hdr . maybe id putBmpPalette opal) $ BS.empty                 
    

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
putBmpPalette (Palette _ arr) = foldl' fn id idxs where
    idxs      = let (l,u) = bounds arr in [l..u]
    fn f idx  = let (r,g,b) = arr!idx in
                f . out1 b . out1 g . out1 r . out1 0  

   
arrayWrite :: Handle -> BmpDibImageData -> IO ()
arrayWrite h arr = do
    arr_b   <- remap arr
    hPutArray h arr_b (width*height)
  where
    remap :: BmpData -> IO (IOUArray Int Word8)
    remap = thaw >=> mapIndices (l,u) fn
    
    width,l,u :: Int
    width  = let ((_,n0),(_,n1))   = bounds arr in 1+(n1-n0)
    height = let ((m0,_),(m1,_))   = bounds arr in 1+(m1-m0)
    (l,u)  = (0,(width*height)-1)          
    
    fn :: Int -> (Int,Int)
    fn i      = i `divMod` width
    
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


