{-# OPTIONS -Wall #-}

module ScaledSquare where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

main :: IO ()
main = do 
  writeEPS_latin1 "./out/scaled_square.eps"  pic1
  writeSVG_latin1 "./out/scaled_square.svg"  pic1


-- both squares should share the bottom left coord...

pic1 :: DPicture
pic1 = frameMulti $ 
    [ uniformScale 0.5 $ square blue 50 (P2 100 100)
    , square red 50 (P2 100 100)
    ]


square :: (Num u, Ord u) => DRGB -> u -> Point2 u -> Primitive u
square rgb sidelen bl = fill rgb $ vertexPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

