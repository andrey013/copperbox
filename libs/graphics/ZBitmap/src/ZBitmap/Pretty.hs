{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print a textual representaton of a Bmp file. 
--
--------------------------------------------------------------------------------


module ZBitmap.Pretty where

import ZBitmap.Datatypes
import ZBitmap.Utils ( fold_lr )

import Data.Array.IArray ( (!) )

import Numeric ( showHex )
import Text.PrettyPrint.HughesPJ



ppBmpHeader :: BmpBitmap -> Doc
ppBmpHeader bmp = (text "BMP Header" $$) $ vcat $ 
    [ anonfield  "magic"       2
    , field      "file_size"   (decHex0x 8 $ fileSizeBmp bmp) 
    , field      "reserved"    (decHex0x 4 r1)
    , field      "reserved"    (decHex0x 4 r2)
    , field      "offset"      (decHex0x 8 $ dataOffsetBmp bmp)
    ]
  where
    (r1,r2) = reservedBytesBmp bmp     

ppBmpDibHeader :: BmpBitmap -> Doc
ppBmpDibHeader bmp = (text "DIB Header" $$) $ vcat $ 
    [ field "header_size"     (decHex0x 8     $ dibSizeBmp bmp)
    , field "image_width"     (integerValue   $ widthBmp bmp)
    , field "image_height"    (integerValue   $ heightBmp bmp)
    , field "colour_planes"   (integerValue   $ colourPlanesBmp bmp)
    , field "bits_per_pixel"  (ppBitsPerPixel $ bitsPerPixelBmp bmp)
    , field "compression"     (ppCompression  $ compressionBmp bmp)
    , field "data_size"       (decHex0x 8     $ imageDataSizeBmp bmp)
    , field "horizontal_res"  (integerValue   $ horizontalResolutionBmp bmp)
    , field "vertical_res"    (integerValue   $ verticalResolutionBmp bmp)
    , field "palette_depth"   (decHex0x 8     $ paletteDepthBmp bmp)
    , field "colours_used"    (decHex0x 8     $ coloursUsedBmp bmp)
    ]

          
ppBitsPerPixel :: BmpBitsPerPixel -> Doc
ppBitsPerPixel B1_Monochrome      = text "1 (mono)"
ppBitsPerPixel B4_Colour16        = text "4 (16 colours)"
ppBitsPerPixel B8_Colour256       = text "8 (256 colours)"
ppBitsPerPixel B16_HighColour     = text "16 (16 bit high colour bitmap)"
ppBitsPerPixel B24_TrueColour24   = text "24 (24 bit true colour bitmap)"
ppBitsPerPixel B32_TrueColour32   = text "32 (32 bit true colour bitmap)"
    
ppCompression :: BmpCompression -> Doc
ppCompression Bi_RGB        = text "BI_RGB"
ppCompression Bi_RLE8       = text "BI_RLE8"
ppCompression Bi_RLE4       = text "BI_RLE4"
ppCompression Bi_BITFIELDS  = text "BI_BITFIELDS"
ppCompression Bi_JPEG       = text "BI_JPEG"
ppCompression Bi_PNG        = text "BI_PNG"



--------------------------------------------------------------------------------
-- Palette

ppPalette :: Palette -> Doc
ppPalette (Palette sz arr) = fold_lr f sz doc1 where
    f i doc = doc $$ (leftpad ' ' 5 $ show i) <> colon <+>  paletteColour (arr!i)
    doc1    = text "Palette" <+> parens (intValue sz <+> text "entries")
    
     
paletteColour :: RgbColour -> Doc
paletteColour (RgbColour r g b) = 
        text "R=" <> fillstring 3 (show r) <> comma 
    <+> text "G=" <> fillstring 3 (show g) <> comma
    <+> text "B=" <> fillstring 3 (show b) <> subscript
  where
    subscript = case (r,g,b) of
        (255,255,255) -> leftpad ' ' 12 "(white)"
        (0,0,0)       -> leftpad ' ' 12 "(black)"    
        _             -> empty


intValue :: Integral a => a -> Doc
intValue = int . fromIntegral 

integerValue :: Integral a => a -> Doc
integerValue = integer . fromIntegral 

decHex0x :: Integral a => Int -> a -> Doc
decHex0x w i = integer (fromIntegral i) <+> parens (ppHex0x w i)


field :: String -> Doc -> Doc
field s d = (rightpad ' ' 18 (' ':s)) <> equals <+> d

anonfield :: String -> Int -> Doc
anonfield s i = field s (text $ replicate i '_')


ppHex0x :: Integral a => Int -> a -> Doc
ppHex0x w i = text "0x" <> ppHex w i

ppHex :: Integral a => Int -> a -> Doc
ppHex w i = leftpad '0' w $ showHex i []

-- Poor mans /fill/ from Daan Leijen\'s PPrint

leftpad :: Char -> Int -> String -> Doc
leftpad ch pad s = let dif = pad - length s in
    text $ replicate dif ch ++ s 

rightpad :: Char -> Int -> String -> Doc
rightpad ch pad s = let dif = pad - length s in
    text $ s ++ replicate dif ch 
    
    
fillstring :: Int -> String -> Doc
fillstring = rightpad ' '

