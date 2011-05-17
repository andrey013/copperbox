{-# OPTIONS -Wall #-}


module Shapes where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.SimpleDots
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
    

shapes :: CtxPicture
shapes = udrawTracing (0::Double) $ do
    draw $ translate 220 10 $ rotate45 $ 
             (borderedShape $ rectangle 90 30) `at` zeroPt -- "Rectangle"
    draw $ (borderedShape $ circle 10) `at` P2 100 0  -- "C0"
   
    localize (stroke_colour red) $ draw $ dotDisk `at` (P2 220 10)
    a <- drawi $ (borderedShape $ diamond 10 10) `at` (P2 40 0) -- "d1"
    redX $ radialAnchor (0.5*pi) a
    draw $ (borderedShape $ rectangle 20 100) `at` (P2 400 50) -- "R2"
    draw $ (borderedShape $ ellipse 20 10) `at` (P2 0 50)
    
    -- Note - the rotate is rotating both the primGraphic and the 
    -- answer. A LocShape cannot be rotated directly.
    --
    draw $ rotateAbout (0.5*pi) tri_ctr  $ (borderedShape $ triangle 20 30)
              `at` tri_ctr
    redX $ tri_ctr
    draw $ (borderedShape $ triangle 20 30) `at` displace (hvec 25) tri_ctr
    redX $ tri_ctr


    draw $ rotateAbout deg45 tri2_ctr $ (borderedShape $ triangle 20 30) 
               `at` displace (hvec 25) tri2_ctr 

    return ()
  where
    tri_ctr     = P2 0 150
    deg45       = d2r $ (45.0::Double)
    tri2_ctr    = P2 100 150 


redX :: (Real u, Floating u, InterpretUnit u) => Anchor u -> TraceDrawing u ()
redX a = localize (stroke_colour red) $ draw $ dotX `at` a
   

    -- NOTE - should coordinates even have a center anchor?
    -- After all you always know where you draw them...