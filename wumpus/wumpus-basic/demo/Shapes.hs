{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Shapes

import Wumpus.Core                              -- package: wumpus-core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU (standardContext 14) shapes
    writeEPS "./out/shapes01.eps" pic1
    writeSVG "./out/shapes01.svg" pic1
    

shapes :: DDrawing
shapes = drawTracing $ do
         _ <- drawi $ borderedShape $ translate 220 10 
                                    $ rotate30
                                    $ rectangle 90 30 $ zeroPt -- "Rectangle"
         _ <- drawi $ borderedShape $ circle 10 $ P2 100 0  -- "C0"
   
         _ <- localize (strokeColour red) $ 
                       drawi $ coordinateDot $ coordinate (P2 220 10)
         a <- drawi $ borderedShape $ diamond 10 10 $ (P2 40 0) -- "d1"
         redCoord $ radialAnchor (0.5*pi) a
         _ <- drawi $ borderedShape $ rectangle 20 100 $ (P2 400 50) -- "R2"
         _ <- drawi $ borderedShape $ ellipse 20 10 $ (P2 0 50)
        
         return ()


redCoord :: (Real u, Floating u, FromPtSize u) => Point2 u -> TraceDrawing u ()
redCoord pt = localize (strokeColour red) $ do 
    _ <- drawi  $ coordinateX $ coordinate $ pt
    return ()

    -- NOTE - should coordinates even have a center anchor?
    -- After all you always know where you draw them...