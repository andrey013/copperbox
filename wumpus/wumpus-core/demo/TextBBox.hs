{-# OPTIONS -Wall #-}

module TextBBox where

import Wumpus.Core
import Wumpus.Core.Text.StandardEncoding


import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/text_bbox.eps" words_pic
    writeSVG "./out/text_bbox.svg" words_pic


peru :: RGBi
peru = RGBi 205  133  63

plum :: RGBi
plum = RGBi 221  160  221

black :: RGBi
black = RGBi 0 0 0 

courier :: FontAttr
courier = FontAttr 48 (FontFace "Courier" "Courier New" 
                                     SVG_REGULAR standard_encoding)


words_pic   :: DPicture
words_pic   = frame $ 
    [ line1, line2, line3, char1, char2 ]
  where
    line1   = boundedCourier "This text is drawn"  (P2  0 180)
    line2   = boundedCourier "within its bounding" (P2  0 120)
    line3   = boundedCourier "box."                (P2  0  60)
    char1   = boundedCourier "1"                   (P2  0   0)
    char2   = boundedCourier "2"                   (P2 29   0)


boundedCourier :: String -> DPoint2 -> DPrimitive
boundedCourier = boundedText courier

boundedText :: (Ord u, PtSize u) 
            => FontAttr -> String -> Point2 u -> Primitive u
boundedText fa@(FontAttr sz _) ss pt = primCat bbox text
  where
    esc_text  = escapeString ss  
    bbox_path = vertexPath $ boundaryCornerList $ textBoundsEsc sz pt esc_text
    bbox      = cstroke peru default_stroke_attr bbox_path
    text      = escapedlabel black fa esc_text pt


