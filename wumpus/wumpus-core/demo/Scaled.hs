{-# OPTIONS -Wall #-}

module Scaled where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/scaled_square.eps"  pic1
    writeSVG_latin1 "./out/scaled_square.svg"  pic1
    writeEPS_latin1 "./out/scaled_circle.eps"  pic2
    writeSVG_latin1 "./out/scaled_circle.svg"  pic2
    writeEPS_latin1 "./out/scaled_label.eps"   pic3
    writeSVG_latin1 "./out/scaled_label.svg"   pic3


-- both squares should share the bottom left corner

pic1 :: DPicture
pic1 = illustrateBounds grey $ frame $ 
    [ uniformScalePrimitive 0.5 $ square blue 50 (P2 100 100)
    , square red 50 (P2 100 100)
    ]


-- both ellipses should share the same center
pic2 :: DPicture
pic2 = illustrateBounds grey $ frame $ 
    [ uniformScalePrimitive 0.5 $ ellipseHH blue 25 (P2 100 100)
    , ellipseHH red 25 (P2 100 100)
    ]


-- both labels should share the same bottom left corner
pic3 :: DPicture
pic3 = illustrateBounds grey $ frame $
    [ uniformScalePrimitive 0.5 $ label blue (P2 100 100)
    , label red (P2 100 100)
    ]



square :: (Num u, Ord u) => RGBi -> u -> Point2 u -> Primitive u
square rgb sidelen bl = fill rgb $ vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

ellipseHH :: Fractional u => RGBi -> u -> Point2 u -> Primitive u
ellipseHH rgb radius ctr = ellipse rgb radius (0.5*radius) ctr
 
label :: Num u => RGBi -> Point2 u -> Primitive u
label rgb bl = textlabel rgb "Wumpus" bl


grey :: RGBi
grey = RGBi 176 197 223


