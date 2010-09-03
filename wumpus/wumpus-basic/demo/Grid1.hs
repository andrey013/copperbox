{-# OPTIONS -Wall #-}

module Grid1 where

import Wumpus.Basic.Colour.SVGColours   -- package: wumpus-basic
import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core

import Data.Maybe
import System.Directory




main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/grid01.eps" pic1
    >> writeSVG_latin1 "./out/grid01.svg" pic1 


pic1 :: Picture Double
pic1 = fromMaybe errK $ drawGraphic $ supply (P2 2.0 2.0) $ border1 `cc` grid1

errK :: a
errK = error "no picture"


border1 :: DGraphicF 
border1 = border red frame1

grid1 :: DGraphicF
grid1 = grid black 20 20 frame1


frame1 :: DRectangle
frame1 = Rectangle 96 56