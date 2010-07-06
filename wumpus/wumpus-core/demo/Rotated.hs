{-# OPTIONS -Wall #-}

module Rotated where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/rot_square.eps"  pic1
    writeSVG_latin1 "./out/rot_square.svg"  pic1
--    writeEPS_latin1 "./out/rot_circle.eps"  pic2
--    writeSVG_latin1 "./out/rot_circle.svg"  pic2
    writeEPS_latin1 "./out/rot_label.eps"   pic3
    writeSVG_latin1 "./out/rot_label.svg"   pic3


-- both squares should share the bottom left coord...

deg45 :: Radian
deg45 = d2r 45

pic1 :: DPicture
pic1 = illustrateBounds grey $ frameMulti $ 
    [ square (rotatePath deg45) blue 50 (P2 100 100)
    , square id red 50 (P2 100 100)
    ]

{-

-- both ellipses should share the same center
pic2 :: DPicture
pic2 = illustrateBounds grey $ frameMulti $ 
    [ rotate45 $ ellipseHH blue 25 (P2 100 100)
    , ellipseHH red 25 (P2 100 100)
    ]
-}

-- both labels should share the same bottom left corner
-- WHOOPS - can't peak into primitive to transform the label....
pic3 :: DPicture
pic3 = illustrateBounds grey $ frameMulti $ 
    [ {- rotateLabel deg45 $ -} label blue (P2 100 100)
    , label red (P2 100 100)
    ]


square :: (Num u, Ord u) => (Path u -> Path u) -> DRGB -> u -> Point2 u -> Primitive u
square trafo rgb sidelen bl = fill rgb $ trafo $ vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]


squarePath :: (Num u, Ord u) => u -> Point2 u -> Path u
squarePath sidelen bl = vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]


ellipseHH :: Fractional u => DRGB -> u -> Point2 u -> Primitive u
ellipseHH rgb radius ctr = ellipse rgb radius (0.5*radius) ctr
 
label :: Num u => DRGB -> Point2 u -> Primitive u
label rgb bl = textlabel rgb "Wumpus" bl


grey :: DRGB
grey = iRGB3 176 197 223


