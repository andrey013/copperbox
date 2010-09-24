{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


module Demo01 where

import Wumpus.Shapes

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors
import Wumpus.Basic.Arrows
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Paths

-- import Wumpus.Basic.SVGColours

import System.Directory

main :: IO ()
main = do 
    { createDirectoryIfMissing True "./out/"
    ; writeEPS_latin1 "./out/demo01.eps" pic1
    ; writeSVG_latin1 "./out/demo01.svg" pic1
    }

infixl 6 `ats` 

ats :: (Translate sh, u ~ DUnit sh) => sh -> Point2 u -> sh
ats sh (P2 x y) = translate x y sh 
    

pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing (standardContext 14) $  mf
  where
    mf = do { r <- drawi $ outlineShape $ rotate30 $ rectangle_ 80 20 "rectangle"
            ; _ <- drawi $ drawShape $ coordinate `ats` (north r)
            ; c <- drawi $ drawShape $ circle_ 30 "circle" `ats` (P2 100 0)
            ; _ <- drawi $ drawShape $ diamond_ 80 40 "diamond"  `ats` (P2 100 60)
            ; e <- drawi $ drawShape $ ellipse_ 40 20 "ellipse"  `ats` (P2 200 0)
            ; _ <- drawi $ drawShape $ freeLabel "free-label"    `ats` (P2 260 60)
            ; _ <- drawi $ arrowPerp connectS `conn` (south c) $ (south e) 
            ; return () 
            }

{-
curvePath :: (Floating u, Ord u) => Point2 u -> Point2 u -> Path u
curvePath p0 p1 = execPath p0 $ 
                     curveto (d2r (270 :: Double)) (d2r (285 :: Double)) p1

-}
