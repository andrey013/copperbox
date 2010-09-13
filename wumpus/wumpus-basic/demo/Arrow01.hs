{-# OPTIONS -Wall #-}

module Arrow01 where

import Wumpus.Basic.Dots
import Wumpus.Basic.Arrows
import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic.Drawing
import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Paths 

import Wumpus.Core                      -- package: wumpus-core

import System.Directory


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/arrow01.eps" pic1
    >> writeSVG_latin1 "./out/arrow01.svg" pic1 

         
pic1 :: Picture Double 
pic1 = execDrawing (standardContext 48) $
    do { _ <- drawAtImg (P2 0 0)    dotDisk
       ; _ <- drawAtImg (P2 100 0)  dotDisk
       ; _ <- drawConnImg (P2 0 0) (P2 100 0) (arrowOTri90 connectS)
       ; _ <- drawAtImg (P2 120 6) (dotText "k") 
       ; _ <- drawAtImg (P2 88  (-20)) (dotText "k") 
       ; _ <- drawConnImg (P2 130 0) (P2 160 0) (arrowPerp connectS)
       ; return () 
       }  

