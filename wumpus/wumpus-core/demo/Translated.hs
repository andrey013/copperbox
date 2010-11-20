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


pic1 :: DPicture
pic1 = illustrateBounds grey $ frame $ 
    [ translate 100 10 $ square blue 50 (P2 100 100)
    , square red 50 (P2 100 100)
    ]

pic2 :: DPicture
pic2 = illustrateBounds grey $ frame $ 
    [ translate 100 10 $ ellipseHH blue 25 (P2 100 100)
    , ellipseHH red 25 (P2 100 100)
    ]

pic3 :: DPicture
pic3 = illustrateBounds grey $ frame $ 
    [ translate 100 10 $ label blue (P2 100 100)
    , label red (P2 100 100)
    ]



square :: (Num u, Ord u) => RGBi -> u -> Point2 u -> Primitive u
square rgb sidelen bl = fill rgb $ vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

ellipseHH :: Fractional u => RGBi -> u -> Point2 u -> Primitive u
ellipseHH rgb radius ctr = fillEllipse rgb radius (0.5*radius) ctr
 
label :: Num u => RGBi -> Point2 u -> Primitive u
label rgb bl = textlabel rgb wumpus_default_font "Wumpus" bl


grey :: RGBi
grey = RGBi 176 197 223



