{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Shapes

import Wumpus.Core                                  -- package: wumpus-core
import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic hiding ( DRectangle )   -- package: wumpus-basic
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.TurtleMonad
import Wumpus.Basic.Paths
import Wumpus.Basic.Paths.Base
import Wumpus.Basic.Paths.Construction

-- import Wumpus.Basic.SVGColours

import System.Directory

main :: IO ()
main = do 
    { createDirectoryIfMissing True "./out/"
    ; writeEPS_latin1 "./out/demo01.eps" pic1
    ; writeSVG_latin1 "./out/demo01.svg" pic1
    }


    

pic1 :: DPicture
pic1 = drawGraphicU $ 
         execTurtleDrawing (regularConfig 40) (0,0) (standardAttr 14) $ mf
  where
    mf = do { r <- node $ draw $ rotate30 $ translate 0 50 
                                            $ rectangle_ 80 20 "rectangle"
            ; _ <- node $ (draw $ coordinate) `at` (north r)
            ; c <- node $ (draw $ circle_ 30 "circle")       `at` (P2 100 0)
            ; _ <- node $ (draw $ diamond_ 80 40 "diamond")  `at` (P2 100 60)
            ; e <- node $ (draw $ ellipse_ 40 20 "ellipse")  `at` (P2 200 0)
            ; _ <- node $ (draw $ freeLabel "free-label")    `at` (P2 200 60)
            ; _ <- connect (arrowOTri45 curvePath) (south c) (south e) 
            ; 
            ; return ()
            }

curvePath :: (Floating u, Ord u) => Point2 u -> Point2 u -> BPath u
curvePath p0 p1 = execPath p0 $ 
                     curveto (d2r (270 :: Double)) (d2r (285 :: Double)) p1


