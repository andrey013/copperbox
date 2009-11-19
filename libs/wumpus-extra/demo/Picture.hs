{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours
import Wumpus.Geometry

main :: IO ()
main = sequence_ [ demo01, demo02 ]


colouredSquare :: DRGB -> Double -> Picture Double
colouredSquare c sz = 
  frame $ fill c $ extractPath $ square sz zeroPt 


demo01 :: IO ()
demo01 = do 
    writeEPS "./out/picture01.eps" Nothing pic1 
    writeSVG "./out/picture01.svg" pic1 
  where
    pic1 = uniformScale 5 $ d1 `over` d2
                               `over` d3 
                               `over` d4
                               `over` colouredSquare cadetBlue 50
    d1   = dotX         black   $ P2 10 10
    d2   = dotPlus      black   $ P2 20 20
    d3   = dotDiamond   black   $ P2 40 20 
    d4   = dotDisk      black   $ P2 20 30


demo02 :: IO ()
demo02 = do 
    writeEPS "./out/picture02.eps" Nothing pic1 
    writeSVG "./out/picture02.svg" pic1 
  where
    pic1 = uniformScale 5 $ backgroundFill cornflowerBlue 
                          $ blankPicture (BBox zeroPt (P2 100 100))
