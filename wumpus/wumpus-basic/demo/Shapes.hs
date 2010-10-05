{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Shapes.Base
import Wumpus.Basic.Shapes.Derived

import Wumpus.Core                              -- package: wumpus-core


import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/shapes01.eps" pic1
    writeSVG_latin1 "./out/shapes01.svg" pic1
    

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing (standardContext 14) $ do
         _ <- drawi $ drawShape $ translate 220 10 $ rotate30
                                                   $ lrectangle 90 30 "Rectangle"
         _ <- drawi $ drawShape $ translate 100  0 $ lcircle 10 "C0"
   
         _ <- localize (strokeColour red) $ 
                       drawi $ drawShape $ translate 220 10 $ rotate30 $ coordinate
         _ <- drawi $ drawShape $ translate 0   40 $ ldiamond 10 10 "d1"
         _ <- drawi $ drawShape $ translate 400 50 $ lrectangle 20 100 "R2"
         return ()


