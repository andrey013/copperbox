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
    

shapes :: CtxPicture
shapes = udrawTracing (0::Double) $ do
    drawi_ $ translate 220 10 $ rotate45 $ 
             (borderedShape $ rectangle 90 30) `at` zeroPt -- "Rectangle"
    drawi_ $ (borderedShape $ circle 10) `at` P2 100 0  -- "C0"
   
    localize (stroke_colour red) $ draw $ markDisk `at` (P2 220 10)
    a <- drawi $ (borderedShape $ diamond 10 10) `at` (P2 40 0) -- "d1"
    redX $ radialAnchor (0.5*pi) a
    drawi_ $ (borderedShape $ rectangle 20 100) `at` (P2 400 50) -- "R2"
    drawi_ $ (borderedShape $ ellipse 20 10) `at` (P2 0 50)
    
    -- Note - the rotate is rotating both the primGraphic and the 
    -- answer. A LocShape cannot be rotated directly.
    --
    drawi_ $ (rotateAbout (0.5*pi) tri_ctr  $ borderedShape $ triangle 20 30) 
              `at` tri_ctr
    redX $ return tri_ctr
    drawi_ $ (borderedShape $ triangle 20 30) `at` displaceVec (hvec 25) tri_ctr
    redX $ return tri_ctr


    drawi_ $ (rotateAbout deg45 tri2_ctr $ borderedShape $ triangle 20 30) 
               `at` displaceVec (hvec 25) tri2_ctr 

    return ()
  where
    tri_ctr     = P2 0 150
    deg45       = d2r $ (45.0::Double)
    tri2_ctr    = P2 100 150 


redX :: (Real u, Floating u, InterpretUnit u) => Anchor u -> TraceDrawing u ()
redX a = localize (stroke_colour red) $ draw $ a >>= \a1 -> markX `at` a1
   

    -- NOTE - should coordinates even have a center anchor?
    -- After all you always know where you draw them...