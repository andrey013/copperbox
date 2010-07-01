{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts           #-}


module RoundCorners where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours

import System.Directory

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/"
    sequence_ [ demo01 ]



demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/rounded.eps" pic1 
    writeSVG_latin1 "./out/rounded.svg" pic1 
  where
    pic1 :: Picture Double
    pic1 = frame $ 
             strokeRoundedPolygon 5 std_attr $ square 40 (P2 0 0) 

    std_attr = (black, LineWidth 1.0)

