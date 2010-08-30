{-# OPTIONS -Wall #-}

module FontMetrics where

import Wumpus.Fresh

import Data.AffineSpace                 -- package: vector-space

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/font_metrics.eps" metrics_pic
    writeSVG_latin1 "./out/font_metrics.svg" metrics_pic



peru :: RGB255
peru = RGB255 205  133  63

plum :: RGB255
plum = RGB255 221  160  221

black :: RGB255
black = RGB255 0 0 0 

courier_attr :: FontAttr
courier_attr = FontAttr 48 (FontFace "Courier" "Courier New" SVG_REGULAR)

metrics_pic :: DPicture
metrics_pic = char_pic `picOver` lines_pic

lines_pic   :: DPicture
lines_pic   = frame $ 
    [ ascender_line, numeral_line, xheight_line, baseline, descender_line ]
  where
    descender_pos   = 0 - courier48_descender_depth
  
    ascender_line   = haxis peru (descender_pos + courier48_height)
    numeral_line    = haxis peru courier48_numeral_height
    xheight_line    = haxis peru courier48_xheight
    baseline        = haxis peru 0
    descender_line  = haxis peru descender_pos



char_pic :: Picture Double
char_pic = frame $ zipWith ($) chars (iterate (.+^ hvec 32) zeroPt)
  where
    chars = (map letter "ABXabdgjxy12") ++ [agraveU]

type PrimF = DPoint2 -> DPrimitive

bodyHeight  :: PrimF
bodyHeight  = vertLine peru $ fromPtSize courier48_numeral_height

agraveU     :: PrimF
agraveU     = textlabel (black, courier_attr) "&#Agrave"

letter :: Char -> DPoint2 -> DPrimitive
letter ch pt = textlabel (black, courier_attr) [ch] pt


vertLine :: RGB255 -> Double -> DPoint2 -> DPrimitive
vertLine rgb height pt = ostroke rgb $ vertexPath [pt, pt .+^ vvec height]

haxis :: RGB255 -> PtSize -> DPrimitive
haxis rgb ypos = 
    ostroke (rgb, dash_attr) $ vertexPath [ pt, pt .+^ hvec 440 ]
  where
    dash_attr = DashPattern (Dash 0 [(2,2)])
    pt        = P2 0 (fromPtSize ypos)