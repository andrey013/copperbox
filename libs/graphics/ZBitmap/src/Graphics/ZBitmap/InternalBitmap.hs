{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.InternalBitmap
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Internal bitmap representation
--
--------------------------------------------------------------------------------

module Graphics.ZBitmap.InternalBitmap where

import Graphics.ZBitmap.InternalSyntax
import Graphics.ZBitmap.Utils 
import Data.Array.Unboxed hiding ( (!) )
import Data.Bits
import Data.Word

import qualified Data.Array.Unboxed as U

(!) :: (Show i, IArray a e, Ix i) => a i e -> i -> e
(!) a i = let (l,u) = bounds a in 
          if l <= i && i <= u then (U.!) a i 
                              else error $ "IB: - arr " ++ show i ++ " " ++ show (l,u)


type ByteCount  = Int
type PixelCount = Int

data Bitmap pictype = Bitmap 
      { picture_width :: PixelCount
      , opt_palette   :: Maybe PaletteData
      , picture_array :: BmpData
      -- 
      , storage_size  :: ByteCount
      , colour_at     :: (Int,Int) -> RgbColour
      }

class BitmapImage a

-- universal Bitmap type
data UniBitmap = forall a. BitmapImage a => 
                    UniBitmap BmpBitsPerPixel (Bitmap a)


instance Show UniBitmap where
  show (UniBitmap bpp bmp) = "UniBitmap "     ++ show bpp 
                              ++ " {width="   ++ show (imageWidth bmp)
                              ++ ", height= " ++ show (imageHeight bmp)
                              ++ "}"    




-- The number of rows in the bmp data array, reflects the number of rows 
-- (aka height) of the image directly. 
-- Remember - add 1 to bounds
imageHeight :: BitmapImage a => Bitmap a -> Int
imageHeight (Bitmap {picture_array=arr}) = 
    let ((r0,_),(r1,_)) = bounds arr in 1 + r1 - r0

-- We must store the image width explicitly - it /cannot/ be recovered from 
-- the array bounds because the array may contain padding.
imageWidth :: BitmapImage a => Bitmap a -> Int
imageWidth = picture_width


      

colourAt :: BitmapImage a => Bitmap a -> ((Int,Int) -> RgbColour)
colourAt = colour_at

uniBitmap :: BmpBitmap -> UniBitmap
uniBitmap bmp = case bitsPerPixel bmp of 
    B1_Monochrome     -> UniBitmap B1_Monochrome  $ bitmapMono'  bmp  
    B4_Colour16       -> UniBitmap B4_Colour16    $ bitmap4bit'  bmp     
    B8_Colour256      -> UniBitmap B8_Colour256   $ bitmap8bit'  bmp      
    B16_HighColour    -> UniBitmap B16_HighColour $ bitmap16bit' bmp    
    B24_TrueColour    -> UniBitmap B24_TrueColour $ bitmap24bit' bmp 
    B32_TrueColour    -> UniBitmap B32_TrueColour $ bitmap32bit' bmp 


-- Extract the essential contents of a UniBitmap stripping off the 
-- (phantom) type layer on the bitmap data.
extractBitmap :: UniBitmap 
              -> ((Int,Int),BmpBitsPerPixel,Maybe Palette,BmpData)
extractBitmap (UniBitmap bpp bmp@(Bitmap {opt_palette=op,picture_array=a})) = 
    ((imageWidth bmp,imageHeight bmp),bpp,fn op,a)
  where
    fn = maybe Nothing (Just . makePalette)     


-- Each bit is a pixel so 8 pixels per byte, rows must be 'quad-aligned'
-- with padding.       
data ImageMono

-- Each 4 bits is a index into the palette, so each byte represents 2 pixels.
-- The Palette contains 16 colours. The rows will be padded for quad alignment 
-- if necessary.       
data Image4bit


-- Each byte is a index into the palette. The Palette naturally contains 256
-- entries. Padded for quad alignment if necessary.       
data Image8bit

-- A colour is represented by 5 bits {5}{5}{5} leaving 1 bit unused.
-- Padded to be quad aligned if necessary.
data Image16bit


-- Each byte represents a colour, strangely the order is {b}{g}{r}
-- There may be padding at the end of a row to get quad alignment.
data Image24bit

-- Each byte represents a colour, the order is {b}{g}{r}{unused}
-- No padding as data is naturally quad aligned for all widths
data Image32bit




instance BitmapImage ImageMono
instance BitmapImage Image4bit
instance BitmapImage Image8bit
instance BitmapImage Image16bit
instance BitmapImage Image24bit
instance BitmapImage Image32bit

   
--------------------------------------------------------------------------------
-- constructors

bitmapMono' :: BmpBitmap -> Bitmap ImageMono 
bitmapMono' bmp = bitmapMono w p d where
    (w,p,d) = widthPaletteAndData bmp        


    
    
bitmapMono :: Int -> PaletteData -> BmpData -> Bitmap ImageMono  
bitmapMono w p d = Bitmap
    { picture_width = w
    , opt_palette   = Just p
    , picture_array = d
    --
    , storage_size  = 4 * ((w + 31) `div` 32)
    , colour_at     = colourAtMono p d
    }
 

bitmap4bit' :: BmpBitmap -> Bitmap Image4bit 
bitmap4bit' bmp = bitmap4bit w p d where
    (w,p,d) = widthPaletteAndData bmp
    
      
bitmap4bit :: Int -> PaletteData -> BmpData -> Bitmap Image4bit  
bitmap4bit w p d = Bitmap 
    { picture_width = w
    , opt_palette   = Just p
    , picture_array = d
    --
    , storage_size  = 4 * ((4 * w + 31) `div` 32) 
    , colour_at     = colourAt4bit p d
    }

bitmap8bit' :: BmpBitmap -> Bitmap Image8bit 
bitmap8bit' bmp = bitmap8bit w p d where
    (w,p,d) = widthPaletteAndData bmp
    
bitmap8bit :: Int -> PaletteData -> BmpData -> Bitmap Image8bit  
bitmap8bit w p d = Bitmap
    { picture_width = w
    , opt_palette   = Just p
    , picture_array = d
    --
    , storage_size  = 4 * ((8 * w + 31) `div` 32) 
    , colour_at     = colourAt8bit p d
    }

-- no palettes for these resolutions
bitmap16bit' :: BmpBitmap -> Bitmap Image16bit 
bitmap16bit' bmp = bitmap16bit w d where
    (w,d) = widthAndData bmp
    
bitmap16bit :: Int -> BmpData -> Bitmap Image16bit  
bitmap16bit w d = Bitmap 
    { picture_width = w
    , opt_palette   = Nothing
    , picture_array = d
    --
    , storage_size  = 4 * ((16 * w + 31) `div` 32)
    , colour_at     = colourAt16bit d
    }


bitmap24bit' :: BmpBitmap -> Bitmap Image24bit 
bitmap24bit' bmp = bitmap24bit w d where
    (w,d) = widthAndData bmp
      
bitmap24bit :: Int -> BmpData -> Bitmap Image24bit  
bitmap24bit w d = Bitmap
    { picture_width = w
    , opt_palette   = Nothing
    , picture_array = d
    --
    , storage_size  = 4 * ((24 * w + 31) `div` 32) 
    , colour_at     = colourAt24bit d
    }

bitmap32bit' :: BmpBitmap -> Bitmap Image32bit 
bitmap32bit' bmp = bitmap32bit w d where
    (w,d) = widthAndData bmp
    
bitmap32bit :: Int -> BmpData -> Bitmap Image32bit  
bitmap32bit w d = Bitmap
    { picture_width = w
    , opt_palette   = Nothing
    , picture_array = d
    --
    , storage_size  = 4 * w
    , colour_at     = colourAt32bit d
    } 

widthAndData :: BmpBitmap -> (Int,BmpData)
widthAndData bmp = (w,d) where
    w = fromIntegral $ bmp_width $ bmp_dibheader $ bmp 
    d = maybe (error $ "no bitmap data") id (bmp_opt_body bmp)
    
widthPaletteAndData :: BmpBitmap -> (Int,PaletteData,BmpData)
widthPaletteAndData bmp = (w,p,d) where
    w = fromIntegral $ bmp_width $ bmp_dibheader $ bmp 
    p = maybe (error $ "no palette data") palette_data (optPalette bmp)
    d = maybe (error $ "no bitmap data")  id           (bmp_opt_body bmp)
    
    

colourAtMono :: PaletteData -> BmpData -> (Int,Int) -> RgbColour
colourAtMono p arr (row,col) = 
    case arr!(r,c) `testBit` ci of
      False  -> paletteColour (0::Int) p
      True   -> paletteColour (1::Int) p
  where
    (r,(c,ci))  = (row, rowDivMod col)
    rowDivMod   = (`divMod` 8)
 

colourAt4bit:: PaletteData -> BmpData -> (Int,Int) -> RgbColour
colourAt4bit p arr (row,col) = 
    let v = arr!(r,c) 
    in case ci of 
      0 -> paletteColour (msnibble v) p
      _ -> paletteColour (lsnibble v) p
  where
    (r,(c,ci))  = (row, rowDivMod col)
    rowDivMod   = (`divMod` 2)

msnibble :: Word8 -> Word8
msnibble = (`shiftL` 4) . (.&.  0xf0)

lsnibble :: Word8 -> Word8
lsnibble = (.&.  0x0f)


colourAt8bit:: PaletteData -> BmpData -> (Int,Int) -> RgbColour
colourAt8bit p arr (row,col) = 
    let v = arr!(row,col) in paletteColour v p



colourAt16bit:: BmpData -> (Int,Int) -> RgbColour
colourAt16bit arr (row,col) = decodeRGB16bit v1 v2 where
    (r,c)  = (row,col*2)
    v1     = arr!(r,c) 
    v2     = arr!(r,c+1)
    
    
      
colourAt24bit:: BmpData -> (Int,Int) -> RgbColour
colourAt24bit arr (row,col) = (red,grn,blu) where
    (r,c) = (row,col*3)
    red   = arr!(r,c)
    grn   = arr!(r,c+1)
    blu   = arr!(r,c+2)

-- note this may be wrong and I need to check... 
-- It might be the way to do it is ignore c and take (c+1),(c+2),(c+3)    
colourAt32bit :: BmpData -> (Int,Int) -> RgbColour
colourAt32bit arr (row,col) = (red,grn,blu) where
    (r,c)  = (row,col*4)
    red   = arr!(r,c)
    grn   = arr!(r,c+1)
    blu   = arr!(r,c+2)
    
    
paletteColour :: Integral a => a -> PaletteData -> RgbColour
paletteColour i p = p!(fromIntegral $ i)  

