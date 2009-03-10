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


module Graphics.ZBitmap.InternalSyntax where

import Data.Array.IArray ( Array, bounds )
import Data.Array.Unboxed ( UArray )

import Data.Word 


--------------------------------------------------------------------------------
-- Data types for BMP files

-- Literal are wrapped with the value they should have.
-- This helps parsing the bitmap header even if it has bad values. 

data BmpLiteral a = BmpLiteral a a 
  deriving (Eq,Show) 

checkLiteral :: Eq a => BmpLiteral a -> Bool
checkLiteral (BmpLiteral a l) = a == l 

literalValue :: BmpLiteral a -> a
literalValue (BmpLiteral a _) = a

-- When writing a BmpLiteral always use the standard value in case
-- the literal has been constructed incorrectly
literalLiteral :: BmpLiteral a -> a
literalLiteral (BmpLiteral _ b) = b


data BmpBitmap = BmpBitmap 
      { bmp_header        :: BmpHeader       
      , opt_palette       :: Maybe Palette
      , opt_pixel_data    :: Maybe PixelData -- e.g. cannot parse due to compression
      }

instance Show BmpBitmap where
  show (BmpBitmap h p _) = "BmpBitmap " ++ show h ++ " " ++ show p ++ " {}"


   
data BmpHeader = BmpHeader 
      { magic               :: MagicNumber
      , bmp_file_size       :: Word32
      , reserved_data       :: ReservedData
        -- start of the image data after the headers and palette
      , image_data_offset   :: Word32
      , dib_header          :: BmpDibHeader   
    }
  deriving Show



type MagicNumber = BmpLiteral (Char,Char)
type ReservedData = (Word16,Word16)


magicNumber :: Char -> Char -> MagicNumber
magicNumber c c' = BmpLiteral (c,c') ('B','M')



-- V3 only 
data BmpDibHeader = BmpDibHeader 
      { dib_size          :: HeaderSize
      , bmp_width         :: Word32
      , bmp_height        :: Word32
      , colour_planes     :: ColourPlanes
      , bits_per_pixel    :: BmpBitsPerPixel
      , compression_type  :: BmpCompression
      , image_data_size   :: Word32
      , h_resolution      :: Word32
      , v_resolution      :: Word32
      , palette_depth     :: Word32
      , colours_used      :: ImportantColours
      }
  deriving Show 

type HeaderSize   = BmpLiteral Word32

headerSize :: Word32 -> HeaderSize
headerSize a = BmpLiteral a 40        -- 40 bytes in the dib header

type ColourPlanes = BmpLiteral Word16

colourPlanes :: Word16 -> ColourPlanes
colourPlanes a = BmpLiteral a 1         -- only 1 colour plane in a bitmap

type ImportantColours = BmpLiteral Word32 

importantColours :: Word32 -> ImportantColours
importantColours a = BmpLiteral a 0       -- all colours important

data Palette = Palette 
      { colour_count    :: Int
      , palette_data    :: PaletteData
      }
      
      
type RgbColour = (Word8,Word8,Word8) 


type PaletteData = Array Int RgbColour

instance Show Palette where
  show (Palette i _) = "Palette " ++ show i ++ "{}"
   


type PixelData = UArray (Int,Int) Word8



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
    
bitsPerPixel :: BmpBitmap -> BmpBitsPerPixel
bitsPerPixel =  bits_per_pixel . dib_header . bmp_header

maybePalette :: BmpBitmap -> Maybe Palette
maybePalette = opt_palette





--------------------------------------------------------------------------------
-- Wrapped constructors

makePalette :: PaletteData -> Palette
makePalette a = let (lo,hi) = bounds a in Palette (1+hi-lo) a 



makeBmpHeaderShort :: Word32 -> Word32 -> BmpDibHeader -> BmpHeader
makeBmpHeaderShort palette_size image_size dib = 
    BmpHeader (magicNumber 'B' 'M') total_size (0,0) offset dib
  where
    hdr_size      = 14
    dib_hdr_size  = 40 
    total_size    = hdr_size + dib_hdr_size + palette_size + image_size
    offset        = hdr_size + dib_hdr_size + palette_size 




-- warning careful with sz

makeBmpDibHeaderShort :: Word32 -> Word32 -> BmpBitsPerPixel -> Word32
                      -> BmpDibHeader
makeBmpDibHeaderShort w h bpp sz = 
    BmpDibHeader hdr_size w h clr_planes bpp Bi_RGB sz 0 0 0 imp_colours 
  where
    hdr_size    = headerSize 40
    clr_planes  = colourPlanes 1
    imp_colours = importantColours 0

     

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


