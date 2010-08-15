{-# OPTIONS -Wall #-}

module Arrow01 where

import Wumpus.Basic.Dots
import Wumpus.Basic.Arrows
import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.TurtleMonad
import Wumpus.Basic.Paths 

import Wumpus.Core                      -- package: wumpus-core

import System.Directory




main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/arrow01.eps" pic1
    >> writeSVG_latin1 "./out/arrow01.svg" pic1 
    >> writeEPS_latin1 "./out/arrow02.eps" pic2
    >> writeSVG_latin1 "./out/arrow02.svg" pic2 


std_attr :: DrawingAttr
std_attr = standardAttr 19

pic1 :: Picture Double
pic1 = drawGraphicU $ 
           tri90 0 std_attr (P2 10 0)
         . tri60 0 std_attr (P2 20 0)
         . tri45 0 std_attr (P2 30 0)
         . textline (textAttr $ std_attr) "sample" (P2 30 (-4))
         . otri90 0 std_attr (P2 110 0)
         . otri60 0 std_attr (P2 120 0)
         . otri45 0 std_attr (P2 130 0)
           
         
pic2 :: Picture Double 
pic2 = drawGraphicU $ 
          execDrawing (standardAttr 48) $
       do { _ <- nodeAt dotDisk  (P2 0 0) 
          ; _ <- nodeAt dotDisk  (P2 100 0)  
          ; _ <- connect (arrowOTri90 connectS) (P2 0 0) (P2 100 0) 
          ; _ <- nodeAt (dotText "k") (P2 120 6)
          ; _ <- nodeAt (dotText "k") (P2 88  (-20))
          ; _ <- connect_ ultrathick  (arrowPerp connectS) (P2 130 0) (P2 160 0) 
          ; return () 
          }  

