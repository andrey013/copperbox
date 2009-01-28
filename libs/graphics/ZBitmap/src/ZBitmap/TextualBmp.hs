{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.TextualBmp
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


module ZBitmap.TextualBmp where

import ZBitmap.Asciitron
import ZBitmap.Datatypes

import Data.Array.IArray ( Array )
import Data.Word 

import Numeric ( showHex )
import Text.PrettyPrint.HughesPJ



ppBMPheader :: BMPheader -> Doc
ppBMPheader (BMPheader sz off) =
    fsep $ [ anonfield  "magic"       2
           , field      "file_size"   (decHex0x 8 sz) 
           , anonfield  "reserved"    1
           , anonfield  "reserved"    1
           , field      "offset"      (decHex0x 8 off)
           ]
    

ppV3DibHeader :: V3Dibheader -> Doc
ppV3DibHeader dib = 
    fsep $ [ field "header_size"      (decHex0x 8 $ _dib_size dib)
           , field "image_width"      (integerValue $ _bmp_width dib)
           , field "image_height"     (integerValue $ _bmp_height dib)
           , field "colour_planes"    (integerValue $ _colour_planes dib)
           , field "bits_per_pixel"   (ppBitsPerPixel $ _bits_per_pixel dib)
           , field "compression"      (ppCompression $ _compression dib)
           , field "data_size"        (decHex0x 8 $ _data_size dib)
           , field "horizontal_res"   (integerValue $ _h_resolution dib)
           , field "vertical_res"     (integerValue $ _v_resolution dib)
           , field "palette_depth"    (decHex0x 8 $ _palette_depth dib)
           , field "colours_used"     (decHex0x 8 $ _colours_used dib)
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

ppBMPbody :: BMPbody -> Doc
ppBMPbody UnrecognizedFormat  = text "Unrecognized Format"
ppBMPbody (RGB24 grid)        = ppColourGrid grid
             

ppColourGrid :: Array (Word32,Word32) RGBcolour -> Doc
ppColourGrid = vcat . map text . quickAsciiHack


ppColourLine :: [RGBcolour] -> Doc
ppColourLine = hsep . map ppRGBcolour


ppRGBcolour :: RGBcolour -> Doc
ppRGBcolour (RGBcolour r g b) = ppHex 2 r <> ppHex 2 g <> ppHex 2 b

integerValue :: Integral a => a -> Doc
integerValue = integer . fromIntegral 

decHex0x :: Integral a => Int -> a -> Doc
decHex0x w i = integer (fromIntegral i) <+> parens (ppHex0x w i)


field :: String -> Doc -> Doc
field s d = text s <> equals <> d

anonfield :: String -> Int -> Doc
anonfield s i = text s <> equals <> (text $ replicate i '_')


ppHex0x :: Integral a => Int -> a -> Doc
ppHex0x w i = text "0x" <> ppHex w i

ppHex :: Integral a => Int -> a -> Doc
ppHex w i = padstring '0' w $ showHex i []

padstring :: Char -> Int -> String -> Doc
padstring ch pad s = let dif = pad - length s in
    text $ replicate dif ch ++ s 
    
    