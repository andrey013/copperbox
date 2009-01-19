{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp.TextualBmp
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


module ZBmp.TextualBmp where

import ZBmp.Asciitron
import ZBmp.Datatypes

import Data.Array.IArray ( Array )
import Data.Word 

import Numeric ( showHex )
import Text.PrettyPrint.HughesPJ



ppBMPheader :: BMPheader -> Doc
ppBMPheader (BMPheader mc sz r1 r2 off) =
    fsep $ [ field "magic"     (doubleQuotes $ text mc) 
           , field "file_size" (decHex0x 8 sz) 
           , field "reserved"  (ppHex0x 4 r1)
           , field "reserved"  (ppHex0x 4 r2)
           , field "offset"    (decHex0x 8 off)
           ]
    

ppDIBheader :: DIBheader -> Doc
ppDIBheader dib = 
    fsep $ [ field "header_size"      (decHex0x 8 $ _dib_size dib)
           , field "image_width"      (integerValue $ _dib_width dib)
           , field "image_height"     (integerValue $ _dib_height dib)
           , field "colour_planes"    (integerValue $ _colour_planes dib)
           , field "bits_per_pixel"   (decHex0x 4 $ _bits_per_pxl dib)
           , field "compression"      (ppCompression $ _compression dib)
           , field "data_size"        (decHex0x 8 $ _data_size dib)
           , field "horizontal_res"   (integerValue $ _h_resolution dib)
           , field "vertical_res"     (integerValue $ _v_resolution dib)
           , field "palette_depth"    (decHex0x 8 $ _palette_depth dib)
           , field "colours_used"     (decHex0x 8 $ _colours_used dib)
           ]

    
    
ppCompression :: Compression -> Doc
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

ppHex0x :: Integral a => Int -> a -> Doc
ppHex0x w i = text "0x" <> ppHex w i

ppHex :: Integral a => Int -> a -> Doc
ppHex w i = padstring '0' w $ showHex i []

padstring :: Char -> Int -> String -> Doc
padstring ch pad s = let dif = pad - length s in
    text $ replicate dif ch ++ s 
    
    