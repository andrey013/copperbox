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

module ZBitmap.Datatypes (
  RowIx, ColIx, TwoDIndex,
  
  PixelCount, ByteCount,
  ImageSize, SurfaceSize, PhysicalSize,
  
  
  Bitmap(..),
  PixelSurface,
  RgbColour(..),
  Palette(..),
  YCbCrColour(..),
  
  BmpBitmap,
  BmpHeader,
  BmpDibHeader,
  BmpPaletteSpec,
  BmpDibImageData,
  BmpBitsPerPixel(..),
  BmpCompression(..),
  
  makeBmpBitmap,
  makeBmpHeaderLong,
  makeBmpHeaderShort,
  makeBmpDibHeaderLong,
  makeBmpDibHeaderShort, 
  
  -- * Querying BmpBitmap attributes
  optPaletteSpecBmp,
  imageDataBmp,
  
  fileSizeBmp,
  reservedBytesBmp,
  dataOffsetBmp,
  
  dibSizeBmp,
  widthBmp,
  heightBmp,
  colourPlanesBmp,
  bitsPerPixelBmp,
  compressionBmp,
  imageDataSizeBmp,
  horizontalResolutionBmp,
  verticalResolutionBmp,
  paletteDepthBmp,
  coloursUsedBmp,

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
type RowIx = Word32 
type ColIx = Word32

type TwoDIndex = (RowIx,ColIx)

type PixelCount  = Word32
type ByteCount   = Word32

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





data Bitmap = Bitmap { 
      bitmap_width      :: Word32,
      bitmap_height     :: Word32,
      surface_width     :: ByteCount,
      bitmap_surface    :: PixelSurface
    }

instance Show Bitmap where
  show (Bitmap w h sw _) = 
        "Bitmap{ width=" ++ show w 
           ++ ", height=" ++ show h 
           ++ ", surface_width=" ++ show sw
           ++ " }"       
    
type PixelSurface = UArray (Word32,Word32) Word8



data RgbColour = RgbColour { 
        _red    :: Word8, 
        _green  :: Word8, 
        _blue   :: Word8 
    }
  deriving Show 

data Palette = Palette { 
        colour_count    :: Word32,
        palette_colours :: Array Word32 RgbColour
      }
      
data YCbCrColour = YCbCrColour { 
      _y_val  :: Float,
      _cb     :: Float,
      _cr     :: Float
    }
  deriving ( Show ) 
     

--------------------------------------------------------------------------------
-- Data types for BMP files

data BmpBitmap = BmpBitmap { 
        _header       :: BmpHeader, 
        _dibheader    :: BmpDibHeader,
        _opt_palette  :: Maybe BmpPaletteSpec,
        _body         :: BmpDibImageData
      }
    deriving Show

   
data BmpHeader = BmpHeader { 
        _file_size  :: Word32,
        _reserved1  :: Word16,
        _reserved2  :: Word16, 
        -- start of the image data after the headers and palette
        _offset     :: Word32   
    }
  deriving Show





-- V3 only 
data BmpDibHeader = BmpDibHeader {
        _dib_size         :: Word32,
        _bmp_width        :: Word32,
        _bmp_height       :: Word32,
        _colour_planes    :: Word16,
        _bits_per_pixel   :: BmpBitsPerPixel,
        _compression      :: BmpCompression,
        _image_data_size  :: Word32,
        _h_resolution     :: Word32,
        _v_resolution     :: Word32,
        _palette_depth    :: Word32,
        _colours_used     :: Word32
    }
  deriving Show 





             
type BmpPaletteSpec = BS.ByteString

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
    

--------------------------------------------------------------------------------
-- Wrapped constructors

makeBmpBitmap :: BmpHeader -> BmpDibHeader 
              -> Maybe BmpPaletteSpec -> BmpDibImageData
              -> BmpBitmap
makeBmpBitmap = BmpBitmap


-- only export this to the Bmp parser not client libraries.        
makeBmpHeaderLong :: Word32 -> Word16 -> Word16 -> Word32 -> BmpHeader
makeBmpHeaderLong = BmpHeader

makeBmpHeaderShort :: Word32 -> Word32 -> BmpHeader
makeBmpHeaderShort palette_size image_size = BmpHeader total_size 0 0 offset
  where
    hdr_size    = 14
    dib_size    = 40 
    total_size  = hdr_size + dib_size + palette_size + image_size
    offset      = hdr_size + dib_size + palette_size 



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
-- Querying an opaque BmpBitmap

optPaletteSpecBmp :: BmpBitmap -> Maybe BmpPaletteSpec
optPaletteSpecBmp (BmpBitmap _ _ o _) = o


imageDataBmp :: BmpBitmap -> BmpDibImageData
imageDataBmp (BmpBitmap _ _ _ d) = d

-- The header

withHeader :: (BmpHeader -> a) -> BmpBitmap -> a
withHeader f (BmpBitmap h _ _ _) = f h

fileSizeBmp :: BmpBitmap -> Word32 
fileSizeBmp = withHeader $ \(BmpHeader sz _ _ _) -> sz

reservedBytesBmp :: BmpBitmap -> (Word16, Word16) 
reservedBytesBmp = withHeader $ \(BmpHeader _ r1 r2 _) -> (r1,r2)

dataOffsetBmp :: BmpBitmap -> Word32 
dataOffsetBmp = withHeader $ \(BmpHeader _ _ _ off) -> off


-- The DIB header

withDibHeader :: (BmpDibHeader -> a) -> BmpBitmap -> a
withDibHeader f (BmpBitmap _ d _ _) = f d

dibSizeBmp        :: BmpBitmap -> Word32  
dibSizeBmp        = withDibHeader $ \dib -> _dib_size dib

widthBmp          :: BmpBitmap -> Word32  
widthBmp          = withDibHeader $ \dib -> _bmp_height dib

heightBmp         :: BmpBitmap -> Word32  
heightBmp         = withDibHeader $ \dib -> _bmp_height dib
        
colourPlanesBmp   :: BmpBitmap -> Word16  
colourPlanesBmp   = withDibHeader $ \dib -> _colour_planes dib

bitsPerPixelBmp   :: BmpBitmap -> BmpBitsPerPixel  
bitsPerPixelBmp   = withDibHeader $ \dib -> _bits_per_pixel dib

compressionBmp            :: BmpBitmap -> BmpCompression  
compressionBmp            = withDibHeader $ \dib -> _compression dib
        
imageDataSizeBmp          :: BmpBitmap -> Word32  
imageDataSizeBmp          = withDibHeader $ \dib -> _image_data_size dib
        
horizontalResolutionBmp   :: BmpBitmap -> Word32  
horizontalResolutionBmp   = withDibHeader $ \dib -> _h_resolution dib

verticalResolutionBmp     :: BmpBitmap -> Word32 
verticalResolutionBmp     = withDibHeader $ \dib -> _v_resolution dib

paletteDepthBmp           :: BmpBitmap -> Word32 
paletteDepthBmp           = withDibHeader $ \dib -> _palette_depth dib
        
coloursUsedBmp     :: BmpBitmap -> Word32 
coloursUsedBmp     = withDibHeader $ \dib -> _colours_used dib        

--------------------------------------------------------------------------------
-- Marshal and unmarshal

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


