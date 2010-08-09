{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Shapes.Base
import Wumpus.Shapes.Circle
import Wumpus.Shapes.Coordinate
import Wumpus.Shapes.Diamond
import Wumpus.Shapes.Rectangle

import Wumpus.Core                                  -- package: wumpus-core
import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic hiding ( DRectangle )   -- package: wumpus-basic
import Wumpus.Basic.Monads.ConsDrawing
-- import Wumpus.Basic.SVGColours

import System.Directory

main :: IO ()
main = do 
    { createDirectoryIfMissing True "./out/"
    ; writeEPS_latin1 "./out/demo01.eps" pic1
    ; writeSVG_latin1 "./out/demo01.svg" pic1
    }


    

pic1 :: DPicture
pic1 = drawGraphicU $ 
         execConsDrawing (regularConfig 40) (0,0) (standardAttr 24) $ mf
  where
    mf = do { r <- liftAG $ draw $ rotate30 $ translate 0 50 
                                            $ rectangle_ 80 20 "rect"
            ; _ <- liftAG $ (draw $ coordinate) `at` (north r)
            ; _ <- liftAG $ (draw $ circle_ 30 "circle")  `at` (P2 100 0)
            ; _ <- liftAG $ (draw $ diamond_ 20 40 "diamond")  `at` (P2 100 40)
            ; return ()
            }


