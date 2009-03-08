{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.Pretty
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


module Graphics.ZBitmap.Pretty (
  ppBmpBitmap,
  ppPalette,
  printCBBox
) where

import Graphics.ZBitmap.InternalSyntax


import Data.Array ( elems )
import Data.List ( foldl' )
import Numeric ( showHex )
import Text.PrettyPrint.HughesPJ

ppBmpBitmap :: BmpBitmap -> Doc
ppBmpBitmap bmp =    (ppBmpHeader     $ bmp_header      bmp) 
                  $$ (ppBmpDibHeader  $ bmp_dibheader   bmp)

ppBmpHeader :: BmpHeader -> Doc
ppBmpHeader hdr = (text "BMP Header" $$) $ vcat $ 
    [ field  "magic"       ((\(c1,c2) -> text [c1,c2]) $ magic hdr)
    , field  "file_size"   (decHex0x 8 $ bmp_file_size      hdr) 
    , field  "reserved"    (decHex0x 4 $ reserved1          hdr)
    , field  "reserved"    (decHex0x 4 $ reserved2          hdr)
    , field  "offset"      (decHex0x 8 $ image_data_offset  hdr)
    ]
   

ppBmpDibHeader :: BmpDibHeader -> Doc
ppBmpDibHeader hdr = (text "DIB Header" $$) $ vcat $ 
    [ field "header_size"     (decHex0x 8     $ dib_size          hdr)
    , field "image_width"     (integerValue   $ bmp_width         hdr)
    , field "image_height"    (integerValue   $ bmp_height        hdr)
    , field "colour_planes"   (integerValue   $ colour_planes     hdr)
    , field "bits_per_pixel"  (ppBitsPerPixel $ bits_per_pixel    hdr)
    , field "compression"     (ppCompression  $ compression_type  hdr)
    , field "data_size"       (decHex0x 8     $ image_data_size   hdr)
    , field "horizontal_res"  (integerValue   $ h_resolution      hdr)
    , field "vertical_res"    (integerValue   $ v_resolution      hdr)
    , field "palette_depth"   (decHex0x 8     $ palette_depth     hdr)
    , field "colours_used"    (decHex0x 8     $ colours_used      hdr)
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
-- Print the coordinates of a bounding box     
    
printCBBox :: (Int,Int) -> (Int,Int) -> IO ()
printCBBox (r0,c0) (r1,c1) = putStrLn $ render $ doc where
    doc =     (indent (cl-x) tl)   <+> text ".."   <+> (indent (cr-y) tr)
           $$ (indent (cl-3) dot)                   <> indent (8 + (cr-y)) dot
           $$ bl                   <+> text ".."   <+> br
    
    dot             = char '.'
    indent i d      = spaces i <> d
    spaces i        = text $ replicate i ' ' 

    (tl,x)          = mkCoord (r0,c0)
    (tr,y)          = mkCoord (r1,c0)
    (bl,cl)         = mkCoord (r0,c1)
    (br,cr)         = mkCoord (r1,c1)
    
    mkCoord :: (Int,Int) -> (Doc,Int)
    mkCoord (i,j)   = let s = show (i,j)  
                      in (text s, length s) 
                      

--------------------------------------------------------------------------------
-- Palette

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

