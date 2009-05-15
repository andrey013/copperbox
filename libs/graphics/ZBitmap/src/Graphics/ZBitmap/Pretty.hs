{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.Pretty
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print a textual representaton of a Bmp file. 
--
--------------------------------------------------------------------------------


module Graphics.ZBitmap.Pretty (
  ppBmpBitmap,
  ppBmpHeader,
  ppPalette,
) where

import Graphics.ZBitmap.InternalSyntax


import Data.Array ( elems )
import Data.List ( foldl' )
import Numeric ( showHex )
import Text.PrettyPrint.HughesPJ

ppBmpBitmap :: BmpBitmap -> Doc
ppBmpBitmap bmp = ppBmpHeader $ bmp_header bmp

ppBmpHeader :: BmpHeader -> Doc
ppBmpHeader (BmpHeader a b c d h) = header $$ ppBmpDibHeader h 
  where 
    header = (text "BMP Header" $$) $ vcat $ 
              [ field  "magic"       (ppLiteral (nspairing char) a)
              , field  "file_size"   (decHex0x 8 b) 
              , field  "reserved"    (nspairing (decHex0x 4) c)
           
              , field  "offset"      (decHex0x 8 d)
              ]
   

ppBmpDibHeader :: BmpDibHeader -> Doc
ppBmpDibHeader (BmpDibHeader a b c d e f g h i j k) =
  (text "DIB Header" $$) $ vcat $ 
    [ field "header_size"     (ppLiteral (decHex0x 8) a)
    , field "image_width"     (integerValue   b)
    , field "image_height"    (integerValue   c)
    , field "colour_planes"   (ppLiteral integerValue d)
    , field "bits_per_pixel"  (ppBitsPerPixel e)
    , field "compression"     (ppCompression  f)
    , field "data_size"       (decHex0x 8     g)
    , field "horizontal_res"  (integerValue   h)
    , field "vertical_res"    (integerValue   i)
    , field "palette_depth"   (decHex0x 8     j)
    , field "colours_used"    (ppLiteral (decHex0x 8) k)
    ]

          
ppBitsPerPixel :: BmpBitsPerPixel -> Doc
ppBitsPerPixel B1_Monochrome      = text "1 (mono)"
ppBitsPerPixel B4_Colour16        = text "4 (16 colours)"
ppBitsPerPixel B8_Colour256       = text "8 (256 colours)"
ppBitsPerPixel B16_HighColour     = text "16 (16 bit high colour bitmap)"
ppBitsPerPixel B24_TrueColour     = text "24 (24 bit true colour bitmap)"
ppBitsPerPixel B32_TrueColour     = text "32 (32 bit true colour bitmap)"
    
ppCompression :: BmpCompression -> Doc
ppCompression Bi_RGB        = text "BI_RGB"
ppCompression Bi_RLE8       = text "BI_RLE8"
ppCompression Bi_RLE4       = text "BI_RLE4"
ppCompression Bi_BITFIELDS  = text "BI_BITFIELDS"
ppCompression Bi_JPEG       = text "BI_JPEG"
ppCompression Bi_PNG        = text "BI_PNG"


                      

--------------------------------------------------------------------------------
-- Palette

ppLiteral :: Eq a => (a -> Doc) -> BmpLiteral a -> Doc 
ppLiteral f v | checkLiteral v  = f $ literalValue v
              | otherwise       = text "WARNING: value" <+> f (literalValue v)
                                 <+> text ", expecting" <+> f (literalLiteral v)


ppPalette :: Palette -> Doc
ppPalette (Palette sz arr) = snd $ foldl' f (0,doc1) $ elems arr 
  where
    f :: (Int,Doc) -> RgbColour -> (Int,Doc)
    f (i,doc) e = (i+1, doc $$ (leftpad ' ' 5 $ show i) 
                            <> colon <+>  paletteColour e)
    doc1    = text "Palette" <+> parens (intValue sz <+> text "entries")
    
     
paletteColour :: RgbColour -> Doc
paletteColour (r,g,b) = 
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

-- no space pairing
nspairing :: (a -> Doc) -> (a,a) -> Doc
nspairing f (a,b) = f a <> f b

