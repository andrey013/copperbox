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
pic1 = fromMaybe errK $ drawGraphic $ supply (P2 2.0 2.0) $ border1 `cc` grid1

errK :: a
errK = error "no picture"

supply :: Point2 u -> GraphicF u -> Graphic u
supply pt g = g pt

border1 :: DGraphicF 
border1 = border red frame1

grid1 :: DGraphicF
grid1 = grid black 20 20 frame1


frame1 :: DRectFrame
frame1 = RectFrame 96 56