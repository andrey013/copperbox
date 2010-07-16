{-# OPTIONS -Wall #-}

module Translated where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/trans_square.eps"  pic1
    writeSVG_latin1 "./out/trans_square.svg"  pic1
    writeEPS_latin1 "./out/trans_circle.eps"  pic2
    writeSVG_latin1 "./out/trans_circle.svg"  pic2
    writeEPS_latin1 "./out/trans_label.eps"   pic3
    writeSVG_latin1 "./out/trans_label.svg"   pic3


-- both squares should share the bottom left coord...
pic1 :: DPicture
pic1 = illustrateBounds grey $ frameMulti $ 
    [ translatePrimitive 100 10 $ square blue 50 (P2 100 100)
    , square red 50 (P2 100 100)
    ]

pic2 :: DPicture
pic2 = illustrateBounds grey $ frameMulti $ 
    [ translatePrimitive 100 10 $ ellipseHH blue 25 (P2 100 100)
    , ellipseHH red 25 (P2 100 100)
    ]

pic3 :: DPicture
pic3 = illustrateBounds grey $ frameMulti $ 
    [ translatePrimitive 100 10 $ label blue (P2 100 100)
    , label red (P2 100 100)
    ]



square :: (Num u, Ord u) => DRGB -> u -> Point2 u -> Primitive u
square rgb sidelen bl = fill rgb $ vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

ellipseHH :: Fractional u => DRGB -> u -> Point2 u -> Primitive u
ellipseHH rgb radius ctr = ellipse rgb radius (0.5*radius) ctr
 
label :: Num u => DRGB -> Point2 u -> Primitive u
label rgb bl = textlabel rgb "Wumpus" bl


grey :: DRGB
grey = iRGB3 176 197 223


