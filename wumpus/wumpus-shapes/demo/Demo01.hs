{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Shapes.Base
import Wumpus.Shapes.Coordinate
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
            ; liftAG $ (draw $ coordinate) `at` (north r)
            }


{-
-- OLD 


pic1 :: DPicture
pic1 = drawGraphicU $ dot1 . dot2 . rects . circ . diam . ellp
  where
    rects = foldr (\e a -> strokeShape lightSteelBlue e . a)
                  id
                  [ translate 100 0 $ rotate45 rect1
                  , rect1
                  ]

    circ  = strokeShape lightSteelBlue $ circ'
    circ' = rotate45 $ translate 180 0 circ1
   
    diam  = fillShape lightSteelBlue $ translate 280 0 diam1
    ellp  = fillShape lightSteelBlue $ rotate45 $ translate 0 60 ell1

    dot1   = disk green 3 $ pos1
    dot2   = disk red 3   $ north circ'

-- Note - anchors must be take after transformation...

rect1    :: DRectangle 
rect1    = rectangle 80 20 `addLabel` basicLabel "Rectangle"

-- This only /works/ because rect1 is not transformed...
pos1 :: DPoint2
pos1 = east rect1

circ1   :: DCircle 
circ1   = circle 20 `addLabel` basicLabel "Circle"

diam1   :: DDiamond
diam1   = diamond 60 60 `addLabel` basicLabel "Diamond"

ell1    :: DEllipse
ell1    = ellipse 60 30 `addLabel` basicLabel "Ellipse"

-}