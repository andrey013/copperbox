{-# OPTIONS -Wall #-}

module Translated where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/trans_square.eps"  pic1
    writeSVG "./out/trans_square.svg"  pic1
    writeEPS "./out/trans_ellipse.eps"  pic2
    writeSVG "./out/trans_ellipse.svg"  pic2
    writeEPS "./out/trans_label.eps"   pic3
    writeSVG "./out/trans_label.svg"   pic3


-- These are out-of-date for version 0.36.0


pic1 :: Picture
pic1 = illustrateBounds grey $ frame $ 
    [ translate 100 10 $ square blue 50 (P2 100 100)
    , square red 50 (P2 100 100)
    ]

pic2 :: Picture
pic2 = illustrateBounds grey $ frame $ 
    [ translate 100 10 $ ellipseHH blue 25 (P2 100 100)
    , ellipseHH red 25 (P2 100 100)
    ]

pic3 :: Picture
pic3 = illustrateBounds grey $ frame $ 
    [ translate 100 10 $ label blue (P2 100 100)
    , label red (P2 100 100)
    ]



square :: RGBi -> Double -> DPoint2 -> Primitive
square rgb sidelen bl = fill rgb $ vertexPrimPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

ellipseHH :: RGBi -> Double -> DPoint2 -> Primitive
ellipseHH rgb radius ctr = fillEllipse rgb radius (0.5*radius) ctr
 
label :: RGBi -> DPoint2 -> Primitive
label rgb bl = textlabel rgb wumpus_default_font "Wumpus" bl


grey :: RGBi
grey = RGBi 176 197 223



