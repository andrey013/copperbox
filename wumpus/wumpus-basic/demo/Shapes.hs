{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Basic.Anchors
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
         a <- drawi $ borderedShape $ diamond 10 10 `at` (P2 40 0) -- "d1"
         redCoord $ radialAnchor (0.5*pi) a
         _ <- drawi $ borderedShape $ rectangle 20 100 `at` (P2 400 50) -- "R2"
         _ <- drawi $ borderedShape $ ellipse 20 10 `at` (P2 0 50)
        
         return ()


redCoord :: (Real u, Floating u) => Point2 u -> Drawing u ()
redCoord pt = localize (strokeColour red) $ do 
    _ <- drawi $ coordinateMark $ coordinate `at` pt
    return ()

    -- NOTE - should coordinates even have a center anchor?
    -- After all you always know where you draw them...