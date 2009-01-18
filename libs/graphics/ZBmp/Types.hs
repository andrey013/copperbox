

module Types where

import Data.Word

data BMPfile = BMPfile { 
        _header     :: BMPheader, 
        _dibheader  :: DIBheader,
        _img_data24 :: [[RGBcolour]]
      }
    deriving Show


   
   
data BMPheader = BMPheader { 
        _magic      :: String,
        _file_size  :: Word32, 
        _reserved1  :: Word16,
        _reserved2  :: Word16,
        _offset     :: Word32 
    }
  deriving Show
  
-- V3 only 
data DIBheader = DIBheader {
        _dib_size       :: Word32,
        _dib_width      :: Word32,
        _dib_height     :: Word32,
        _colour_planes  :: Word16,
        _bits_per_pxl   :: Word16,
        _compression    :: Compression,
        _data_size      :: Word32,
        _h_resolution   :: Word32,
        _v_resolution   :: Word32,
        _palette_depth  :: Word32,
        _colours_used   :: Word32
    }
  deriving Show 

data RGBcolour = RGBcolour { 
        _red    :: Word8, 
        _green  :: Word8, 
        _blue   :: Word8 
    }
  deriving Show 

data Compression =
      Bi_RGB
    | Bi_RLE8
    | Bi_RLE4
    | Bi_BITFIELDS
    | Bi_JPEG
    | Bi_PNG
    deriving ( Enum, Eq, Ord, Show )
    
    
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