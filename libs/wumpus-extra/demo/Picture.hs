{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours
import Wumpus.Geometry

import Wumpus.Core.Colour  -- To correct in Wumpus.Core.hs

main :: IO ()
main = sequence_ [ demo01 ]


colouredSquare :: DRGB -> Double -> Picture Double
colouredSquare c sz = 
  frame $ fill c $ extractPath $ square sz zeroPt 


demo01 :: IO ()
demo01 = do 
    writeEPS "./out/picture01.eps" (Just ("Courier", 12)) pic1 
    writeSVG "./out/picture01.svg" pic1 
  where
    pic1 = d1 `over` d2 `over` colouredSquare cadetBlue 50
    d1   = dotX    `at` P2 10 10
    d2   = dotPlus `at` P2 20 20

