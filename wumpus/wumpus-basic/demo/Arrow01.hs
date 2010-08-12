{-# OPTIONS -Wall #-}

module Arrow01 where

import Wumpus.Basic.AnchorDots
import Wumpus.Basic.Arrowheads
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingMonad

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
           tri90 std_attr 0 (P2 10 0)
         . tri60 std_attr 0 (P2 20 0)
         . tri45 std_attr 0 (P2 30 0)
         . textline (textAttr $ std_attr) "sample" (P2 30 (-4))
         . otri90 std_attr 0 (P2 110 0)
         . otri60 std_attr 0 (P2 120 0)
         . otri45 std_attr 0 (P2 130 0)
           
         
pic2 :: Picture Double 
pic2 = drawGraphicU $ execDrawing (standardAttr 48) $
       do { _ <- liftAG dotDisk (P2 0 0) 
          ; _ <- liftAG dotDisk (P2 100 0)  
          ; _ <- liftAG2 arrowOTri90 (P2 0 0) (P2 100 0) 
          ; _ <- liftAG (dotText "k") (P2 120 6)
          ; _ <- liftAG (dotText "k") (P2 88  (-20))
          ; _ <- liftAG2 arrowPerp (P2 130 0) (P2 160 0) 
          ; return () 
          }  
