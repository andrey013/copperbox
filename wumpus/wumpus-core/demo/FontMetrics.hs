{-# OPTIONS -Wall #-}

module FontMetrics where

import Wumpus.Core

import Data.AffineSpace                 -- package: vector-space

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/font_metrics.eps" metrics_pic
    writeSVG_latin1 "./out/font_metrics.svg" metrics_pic



peru :: PSRgb
peru = RGB3 0.804  0.522  0.247

plum :: PSRgb
plum = RGB3 0.867  0.627  0.867

black :: PSRgb
black = RGB3 0 0 0 

courier_attr :: FontAttr
courier_attr = FontAttr "Courier" "Courier New" SVG_REGULAR 48

metrics_pic :: DPicture
metrics_pic = char_pic `picOver` lines_pic

lines_pic   :: DPicture
lines_pic   = frameMulti $ 
    [ ascender_line, numeral_line, xheight_line, baseline, descender_line ]
  where
    descender_pos   = 0 - courier48_descender_depth
  
    ascender_line   = haxis peru (descender_pos + courier48_height)
    numeral_line    = haxis peru courier48_numeral_height
    xheight_line    = haxis peru courier48_xheight
    baseline        = haxis peru 0
    descender_line  = haxis peru descender_pos



char_pic :: Picture Double
char_pic = frameMulti $ zipWith ($) chars (iterate (.+^ hvec 32) zeroPt)
  where
    chars = (map letter "ABXabdgjxy12") ++ [agraveU]

type PrimF = DPoint2 -> DPrimitive

bodyHeight  :: PrimF
bodyHeight  = vertLine peru courier48_numeral_height

agraveU     :: PrimF
agraveU     = textlabel (black, courier_attr) "&#Agrave"

letter :: Char -> DPoint2 -> DPrimitive
letter ch pt = textlabel (black, courier_attr) [ch] pt


vertLine :: DRGB -> Double -> DPoint2 -> DPrimitive
vertLine rgb height pt = ostroke rgb $ vertexPath [pt, pt .+^ vvec height]

haxis :: DRGB -> Double -> DPrimitive
haxis rgb ypos = 
    ostroke (rgb, dash_attr) $ vertexPath [ pt, pt .+^ hvec 440 ]
  where
    dash_attr = DashPattern (Dash 0 [(2,2)])
    pt        = P2 0 ypos