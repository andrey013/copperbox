{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Paths
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
         _ <- drawi $ drawShape $ rectangle 80  10
         _ <- drawi $ drawShape $ translate 100  0 $ circle 10
         _ <- drawi $ drawShape $ translate 120  0 $ coordinate
         _ <- drawi $ drawShape $ translate 0   40 $ diamond 10 10
         _ <- drawi $ drawShape $ translate 100 40 $ ellipse 20 10
         return ()
