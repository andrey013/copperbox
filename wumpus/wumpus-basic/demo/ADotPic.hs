{-# OPTIONS -Wall #-}

module ADotPic where


import Wumpus.Basic.Anchors
import Wumpus.Basic.Dots.AnchorDots
import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"    
    let pic1 = runDrawingU std_attr dot_drawing 
    writeEPS "./out/anchor_dots01.eps" pic1
    writeSVG "./out/anchor_dots01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


dot_drawing :: DDrawing
dot_drawing = drawTracing mf 


mf :: (Floating u, FromPtSize u) => TraceDrawing u ()
mf = do 
    a <- drawi $ dotCircle `at` zeroPt
    b <- drawi $ dotCircle `at` (P2 60 60)
    _ <- drawi $ dotCircle `at` (P2 120 0)
    let p1 = radialAnchor (pi/4)    a
    let p2 = radialAnchor (5* pi/4) b
    xdraw link1 $ straightLineBetween p1 p2
  where
    link1 = xlinkhref "http://www.haskell.org"

