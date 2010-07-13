{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Shapes

import Wumpus.Core hiding ( ellipse )           -- package: wumpus-core
import Wumpus.Basic.Graphic hiding ( circle )   -- package: wumpus-basic
import Wumpus.Basic.SVGColours

import Data.Maybe
import System.Directory

main :: IO ()
main = do 
    { createDirectoryIfMissing True "./out/"
    ; writeEPS_latin1 "./out/demo01.eps" pic1
    ; writeSVG_latin1 "./out/demo01.svg" pic1
    }

errK :: a
errK = error "no picture"

pic1 :: DPicture
pic1 = fromMaybe errK $ drawGraphic $ rects . circ . diam . ellp
  where
    rects = foldr (\e a -> strokeShape lightSteelBlue e . a)
                  id
                  [ translate 100 0 $ rotate45 rect1
                  , rect1
                  ]

    circ  = strokeShape lightSteelBlue $ rotate45 $ translate 180 0 circ1
    diam  = fillShape lightSteelBlue $ translate 280 0 diam1
    ellp  = fillShape lightSteelBlue $ rotate45 $ translate 0 60 ell1

rect1    :: DRectangle 
rect1    = rectangle 80 20 `addLabel` basicLabel "Rectangle"


circ1   :: DCircle 
circ1   = circle 20 `addLabel` basicLabel "Circle"

diam1   :: DDiamond
diam1   = diamond 60 60 `addLabel` basicLabel "Diamond"

ell1    :: DEllipse
ell1    = ellipse 60 30 `addLabel` basicLabel "Ellipse"