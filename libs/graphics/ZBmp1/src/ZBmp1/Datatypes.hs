{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp1.Datatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for Bmp files.
--
--------------------------------------------------------------------------------

module ZBmp1.Datatypes where

import Data.Array.Unboxed ( UArray )
import Data.Word 


-- Use C style - (row,column) addressing.
type RowIx = Word32 
type ColIx = Word32

type TwoDIndex = (RowIx,ColIx)

 


data BMPfile = BMPfile { 
        _header         :: BMPheader, 
        _dibheader      :: V3Dibheader,
        _palette_spec   :: PaletteSpec,
        _body           :: BMPbody
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
        _bits_per_pxl   :: BitsPerPixel,
        _compression    :: Compression,
        _data_size      :: Word32,
        _h_resolution   :: Word32,
        _v_resolution   :: Word32,
        _palette_depth  :: Word32,
        _colours_used   :: Word32
    }
  deriving Show 

data PaletteSpec = 
      NoPalette -- this occurs for 16bit 24bit and 32bit bitmaps
    | Palette Int ArrayWord8                 
    deriving Show

type PaletteData = ArrayWord8
    
data BMPbody = UnrecognizedFormat
             | Mono ImageData
    deriving Show


type ImageData = ArrayWord8

type ArrayWord8 = UArray Word32 Word8

data RGBcolour = RGBcolour { 
        _red    :: Word8, 
        _green  :: Word8, 
        _blue   :: Word8 
    }
  deriving Show 

data BitsPerPixel = 
      B1_Monochrome
    | B4_Colour16      
    | B8_Colour256     
    | B16_HighColour   
    | B24_TrueColour24
    | B32_TrueColour32
    deriving ( Enum, Eq, Ord, Show )  

data Compression =
      Bi_RGB
    | Bi_RLE8
    | Bi_RLE4
    | Bi_BITFIELDS
    | Bi_JPEG
    | Bi_PNG
    deriving ( Enum, Eq, Ord, Show )
    

marshalBitsPerPixel :: BitsPerPixel -> Word16
marshalBitsPerPixel x = case x of
      B1_Monochrome     -> 1
      B4_Colour16       -> 4
      B8_Colour256      -> 8
      B16_HighColour    -> 16
      B24_TrueColour24  -> 24
      B32_TrueColour32  -> 32

unmarshalBitsPerPixel :: Word16 -> BitsPerPixel
unmarshalBitsPerPixel x
      | x == 1    = B1_Monochrome  
      | x == 4    = B4_Colour16  
      | x == 8    = B8_Colour256
      | x == 16   = B16_HighColour
      | x == 24   = B24_TrueColour24
      | x == 32   = B32_TrueColour32         
      | otherwise = error ("unmarshalCompression - illegal value " ++ show x)
      
          
marshalCompression :: Compression -> Word32
marshalCompression x = case x of
      Bi_RGB       -> 0
      Bi_RLE8      -> 1
      Bi_RLE4      -> 2
      Bi_BITFIELDS -> 3
      Bi_JPEG      -> 4
      Bi_PNG       -> 5

unmarshalCompression :: Word32 -> Compression
unmarshalCompression x
      | x == 0    = Bi_RGB  
      | x == 1    = Bi_RLE8  
      | x == 2    = Bi_RLE4
      | x == 3    = Bi_BITFIELDS
      | x == 4    = Bi_JPEG
      | x == 5    = Bi_PNG         
      | otherwise = error ("unmarshalCompression - illegal value " ++ show x)


