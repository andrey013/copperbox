{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.InternalSyntax
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


module Graphics.ZBitmap.InternalSyntax (
  RowIx, ColIx, BitmapIndex,
  
  PixelCount, ByteCount,
  ImageSize, SurfaceSize, PhysicalSize,
  
  
  PixelSurface,
  PaletteColour(..),
  PaletteData,
  Palette(..),
  
  BmpBitmap(..),
  BmpHeader(..),
  BmpDibHeader(..),
  BmpDibImageData,
  BmpBitsPerPixel(..),
  BmpCompression(..),
  
  makeBmpHeaderLong,
  makeBmpHeaderShort,
  makeBmpDibHeaderLong,
  makeBmpDibHeaderShort, 
  

  marshalBmpBitsPerPixel,
  unmarshalBmpBitsPerPixel,
  marshalBmpCompression,
  unmarshalBmpCompression
) where

import Data.Array.IArray ( Array )
import Data.Array.Unboxed ( UArray )
import qualified Data.ByteString as BS -- strict Word8 representation 

import Data.Word 



-- Use C style - (row,column) addressing.
type RowIx = Int 
type ColIx = Int

type BitmapIndex = (RowIx,ColIx)

type PixelCount  = Int
type ByteCount   = Int

-- | @ImageSize = (rows,cols)@ - 
-- Image size is the /viewable/ extent of a bitmap.
-- It is the number of adressable vertical pixels x the number 
-- of horizontal pixels. 
type ImageSize    = (PixelCount,PixelCount)

-- | @SurfaceSize = (rows,cols)@ - 
-- Surface size is the /allocated/ extent of a bitmap.
-- Unless the width of the bitmap is a multiple of 8 then then it will have 
-- extra pixels padding at the right end of each row. 
-- The number of rows a surface has will always be the same as the 
-- number of rows the corresponding image has.
type SurfaceSize  = (PixelCount,PixelCount)

-- | @PhysicalSize = (rows,cols)@ -
-- The physical size of a bitmap /in bytes/. 
-- The physical size may be larger or smaller than the surface size,
-- depending on the /bit-depth/ of the image.
--
-- For instance mono bitmaps store 8 pixels in 1 byte, so the physical size
-- will be smaller than the surface size.
-- 
-- 24 bit bitmaps need 3 bytes for a pixel so the the physical size will be 
-- greater than the surface size.
type PhysicalSize = (PixelCount,ByteCount)



type PixelSurface = UArray (Int,Int) Word8
     





      

     

--------------------------------------------------------------------------------
-- Data types for BMP files

data BmpBitmap = BmpBitmap 
      { bmp_header       :: BmpHeader 
      , bmp_dibheader    :: BmpDibHeader
      , bmp_opt_palette  :: Maybe Palette
      , bmp_body         :: BmpDibImageData
      }

instance Show BmpBitmap where
  show (BmpBitmap h d p _) = "BmpBitmap " ++ show h ++ " " ++ show d ++ " "
                                ++ show p ++ " {}"


   
data BmpHeader = BmpHeader 
      { magic               :: (Char,Char)
      , bmp_file_size       :: Word32
      , reserved1           :: Word16
      , reserved2           :: Word16 
        -- start of the image data after the headers and palette
      , image_data_offset   :: Word32   
    }
  deriving Show





-- V3 only 
data BmpDibHeader = BmpDibHeader 
      { dib_size          :: Word32
      , bmp_width         :: Word32
      , bmp_height        :: Word32
      , colour_planes     :: Word16
      , bits_per_pixel    :: BmpBitsPerPixel
      , compression_type  :: BmpCompression
      , image_data_size   :: Word32
      , h_resolution      :: Word32
      , v_resolution      :: Word32
      , palette_depth     :: Word32
      , colours_used      :: Word32
      }
  deriving Show 

data Palette = Palette 
      { colour_count    :: Int
      , palette_data    :: PaletteData
      }
      
      
data PaletteColour = PaletteColour 
      { palette_red     :: Word8 
      , palette_green   :: Word8 
      , palette_blue    :: Word8 
      }
  deriving Show 

type PaletteData = Array Int PaletteColour

instance Show Palette where
  show (Palette i _) = "Palette " ++ show i ++ "{}"
   



type BmpDibImageData = BS.ByteString



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
    | B24_TrueColour
    | B32_TrueColour
    deriving ( Enum, Eq, Ord, Show )  

data BmpCompression =
      Bi_RGB
    | Bi_RLE8
    | Bi_RLE4
    | Bi_BITFIELDS
    | Bi_JPEG
    | Bi_PNG
    deriving ( Enum, Eq, Ord, Show )
    

--------------------------------------------------------------------------------
-- Wrapped constructors


-- only export this to the Bmp parser not client libraries.        
makeBmpHeaderLong :: Char -> Char ->Word32 -> Word16 -> Word16 -> Word32 -> BmpHeader
makeBmpHeaderLong m1 m2 = BmpHeader (m1,m2)

makeBmpHeaderShort :: Word32 -> Word32 -> BmpHeader
makeBmpHeaderShort palette_size image_size = 
    BmpHeader ('B','M') total_size 0 0 offset
  where
    hdr_size      = 14
    dib_hdr_size  = 40 
    total_size    = hdr_size + dib_hdr_size + palette_size + image_size
    offset        = hdr_size + dib_hdr_size + palette_size 



-- only export this to ZBitmap modules not client libraries.
makeBmpDibHeaderLong :: Word32 -> Word32 -> Word16 
                     -> BmpBitsPerPixel -> BmpCompression -> Word32 
                     -> Word32 -> Word32 -> Word32 -> Word32 
                     -> BmpDibHeader
makeBmpDibHeaderLong = BmpDibHeader 40


-- warning careful with sz

makeBmpDibHeaderShort :: Word32 -> Word32 -> BmpBitsPerPixel -> Word32
                      -> BmpDibHeader
makeBmpDibHeaderShort w h bpp sz = BmpDibHeader 40 w h 1 bpp Bi_RGB sz 0 0 0 0


     

--------------------------------------------------------------------------------
-- Marshal and unmarshal

marshalBmpBitsPerPixel :: BmpBitsPerPixel -> Word16
marshalBmpBitsPerPixel x = case x of
      B1_Monochrome     -> 1
      B4_Colour16       -> 4
      B8_Colour256      -> 8
      B16_HighColour    -> 16
      B24_TrueColour    -> 24
      B32_TrueColour    -> 32

unmarshalBmpBitsPerPixel :: Word16 -> BmpBitsPerPixel
unmarshalBmpBitsPerPixel x
      | x == 1    = B1_Monochrome  
      | x == 4    = B4_Colour16  
      | x == 8    = B8_Colour256
      | x == 16   = B16_HighColour
      | x == 24   = B24_TrueColour
      | x == 32   = B32_TrueColour         
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


