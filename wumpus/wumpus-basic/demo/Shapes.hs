{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Shapes

import Wumpus.Core                              -- package: wumpus-core


import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/shapes01.eps" pic1
    writeSVG_latin1 "./out/shapes01.svg" pic1
    

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing (standardContext 14) $ do
         _ <- drawi $ borderedShape $ translate 220 10 
                                    $ rotate30
                                    $ rectangle 90 30 `at` zeroPt -- "Rectangle"
         _ <- drawi $ borderedShape $ circle 10 `at` P2 100 0  -- "C0"
   
         _ <- localize (strokeColour red) $ 
                       drawi $ coordinateMark $ coordinate (P2 220 10)
         _ <- drawi $ borderedShape $ diamond 10 10 `at` (P2 40 0) -- "d1"
         _ <- drawi $ borderedShape $ rectangle 20 100 `at` (P2 400 50) -- "R2"
         return ()


