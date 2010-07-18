{-# OPTIONS -Wall #-}

module Grid1 where

import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SVGColours

import Wumpus.Core                      -- package: wumpus-core

import Data.Maybe
import System.Directory




main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/grid01.eps" pic1
    >> writeSVG_latin1 "./out/grid01.svg" pic1 


pic1 :: Picture Double
pic1 = fromMaybe errK $ drawGraphic grid1

errK :: a
errK = error "no picture"

grid1 :: DGraphic
grid1 = grid black 20 20 (RectFrame 96 56) (P2 2.0 2.0) 
