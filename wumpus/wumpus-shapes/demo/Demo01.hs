{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Shapes.Base
import Wumpus.Shapes.Rectangle

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic
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
pic1 = fromMaybe errK $ drawGraphic $ 
         foldr (\e a -> strokeShape lightSteelBlue e . a) id
         [ translate 100 0 $ rotate45 rect1
         , rect1
         ]


rect1    :: DRectangle 
rect1    = rectangle 80 20 zeroPt `addLabel` "Rectangle"
