{-# OPTIONS -Wall #-}

module RotatedSquare where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

main :: IO ()
main = do 
  writeEPS_latin1 "./out/rot_square.eps"  pic1
  writeSVG_latin1 "./out/rot_square.svg"  pic1


-- both squares should share the bottom left coord...

pic1 :: DPicture
pic1 = frameMulti $ 
    [ rotate (d2r (45.0::Double)) $ square blue 50 (P2 100 100)
    , square red 50 (P2 100 100)
    ]


square :: (Num u, Ord u) => DRGB -> u -> Point2 u -> Primitive u
square rgb sidelen bl = fill rgb $ vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

