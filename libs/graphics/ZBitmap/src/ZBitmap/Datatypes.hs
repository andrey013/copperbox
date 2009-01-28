{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.Datatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for bitmaps and Bmp files.
--
--------------------------------------------------------------------------------

module ZBitmap.Datatypes where

import Data.Array.IArray ( Array )
import Data.Array.Unboxed ( UArray )
import qualified Data.ByteString as BS -- strict Word8 representation 

import Data.Word 



-- Use C style - (row,column) addressing.
type RowIx = Word32 
type ColIx = Word32

type TwoDIndex = (RowIx,ColIx)


data Bitmap i = Bitmap { 
      bitmap_width      :: i,
      bitmap_height     :: i,
      byte_width        :: ByteCount,
      bitmap_surface    :: PixelSurface i
    }

instance Show i => Show (Bitmap i) where
  show (Bitmap w h bw _) = 
      "Bitmap{ width=" ++ show w ++ ", height=" ++ show h 
               ++ ", byte_width=" ++ show (getByteCount bw) ++ " }"       
    
-- 2D array - parametric on the index so the user can choose
-- how they avoid unnecessary fromIntegral conversions.   
type PixelSurface i = UArray (i,i) Word8



newtype ByteCount = ByteCount { getByteCount :: Word32 }
  deriving (Eq,Num,Ord,Show)

data RGBcolour = RGBcolour { 
        _red    :: Word8, 
        _green  :: Word8, 
        _blue   :: Word8 
    }
  deriving Show 



   

--------------------------------------------------------------------------------
-- Data types for BMP files

data BMPfile = BMPfile { 
        _header       :: BMPheader, 
        _dibheader    :: V3Dibheader,
        _opt_palette  :: Maybe PaletteSpec,
        _body         :: DibImageData
      }
    deriving Show


   
data BMPheader = BMPheader { 
        _file_size  :: Word32, 
        _offset     :: Word32 
    }
  deriving Show
  
-- V3 only 
data V3Dibheader = V3Dibheader {
        _dib_size       :: Word32,
        _bmp_width      :: Word32,
        _bmp_height     :: Word32,
        _colour_planes  :: Word16,
        _bits_per_pixel :: BmpBitsPerPixel,
        _compression    :: BmpCompression,
        _data_size      :: Word32,
        _h_resolution   :: Word32,
        _v_resolution   :: Word32,
        _palette_depth  :: Word32,
        _colours_used   :: Word32
    }
  deriving Show 


type PaletteSpec = BS.ByteString

    
type ArrayWord8 = UArray Int Word8
    
data BMPbody = UnrecognizedFormat
             | RGB24 ImageData'
    deriving Show

-- This should really be generalized to an array of Word8
-- with a more abstract indexeing scheme then we can handle 
-- different pixels depths 
type ImageData' = Array (Word32,Word32) RGBcolour

-- move to this...
type DibImageData = BS.ByteString



-- B1_Monochrome    - stores 8 pixels in a word8  - value is idx to colour table
-- B4_Colour16      - stores 2 pixels in a word8  - value is idx to colour table
-- B8_Colour256     - one pixel per word8         - value is idx to colour table
-- B16_HighColour   - one pixel in 2 x word8 - (Bi_RGB only) 
-- B24_TrueColour24 - one pixel in 3 x word8 
-- B32_TrueColour32 - one pixel in 4 x word8 - (Bi_RGB only) - one word8 unused


data BmpBitsPerPixel = 
      B1_Monochrome
    | B4_Colour16      
    | B8_Colour256     
    | B16_HighColour   
    | B24_TrueColour24
    | B32_TrueColour32
    deriving ( Enum, Eq, Ord, Show )  

data BmpCompression =
      Bi_RGB
    | Bi_RLE8
    | Bi_RLE4
    | Bi_BITFIELDS
    | Bi_JPEG
    | Bi_PNG
    deriving ( Enum, Eq, Ord, Show )
    

marshalBmpBitsPerPixel :: BmpBitsPerPixel -> Word16
marshalBmpBitsPerPixel x = case x of
      B1_Monochrome     -> 1
      B4_Colour16       -> 4
      B8_Colour256      -> 8
      B16_HighColour    -> 16
      B24_TrueColour24  -> 24
      B32_TrueColour32  -> 32

unmarshalBmpBitsPerPixel :: Word16 -> BmpBitsPerPixel
unmarshalBmpBitsPerPixel x
      | x == 1    = B1_Monochrome  
      | x == 4    = B4_Colour16  
      | x == 8    = B8_Colour256
      | x == 16   = B16_HighColour
      | x == 24   = B24_TrueColour24
      | x == 32   = B32_TrueColour32         
      | otherwise = error ("unmarshalBmpBitsPerPixel - illegal value " ++ show x)
      
          
marshalBmpCompression :: BmpCompression -> Word32
marshalBmpCompression x = case x of
      Bi_RGB       -> 0
      Bi_RLE8      -> 1
      Bi_RLE4      -> 2
      Bi_BITFIELDS -> 3
      Bi_JPEG      -> 4
      Bi_PNG       -> 5

unmarshalBmpCompression :: Word32 -> BmpCompression
unmarshalBmpCompression x
      | x == 0    = Bi_RGB  
      | x == 1    = Bi_RLE8  
      | x == 2    = Bi_RLE4
      | x == 3    = Bi_BITFIELDS
      | x == 4    = Bi_JPEG
      | x == 5    = Bi_PNG         
      | otherwise = error ("unmarshalBmpCompression - illegal value " ++ show x)


