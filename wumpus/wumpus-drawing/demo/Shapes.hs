{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.Marks
import Wumpus.Drawing.Shapes

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU (standardContext 14) shapes
    writeEPS "./out/shapes01.eps" pic1
    writeSVG "./out/shapes01.svg" pic1
    

shapes :: DCtxPicture
shapes = drawTracing $ do
    drawi_ $ translate 220 10 $ rotate45 $ 
             (borderedShape $ rectangle 90 30) `at` zeroPt -- "Rectangle"
    drawi_ $ (borderedShape $ circle 10) `at` P2 100 0  -- "C0"
   
    localize (strokeColour red) $ draw $ markDisk `at` (P2 220 10)
    a <- drawi $ (borderedShape $ diamond 10 10) `at` (P2 40 0) -- "d1"
    redX $ radialAnchor (0.5*pi) a
    drawi_ $ (borderedShape $ rectangle 20 100) `at` (P2 400 50) -- "R2"
    drawi_ $ (borderedShape $ ellipse 20 10) `at` (P2 0 50)
    
    -- Note - the rotate is rotating both the primGraphic and the 
    -- answer. A LocShape cannot be rotated directly.
    --
    drawi_ $ (rotate (d2r 10) $ borderedShape $ triangle 20 40) `at` P2 0 150
    redX $ P2 0 150

    return ()


redX :: (Real u, Floating u, FromPtSize u) => Point2 u -> TraceDrawing u ()
redX pt = localize (strokeColour red) $ draw $ markX `at` pt
   

    -- NOTE - should coordinates even have a center anchor?
    -- After all you always know where you draw them...