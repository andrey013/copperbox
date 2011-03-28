{-# OPTIONS -Wall #-}

module ADotPic where

import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Basic.Kernel              -- package: wumpus-basic

import Wumpus.Core                      -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"    
    let pic1 = runCtxPictureU std_attr dot_pic
    writeEPS "./out/adot_pic01.eps" pic1
    writeSVG "./out/adot_pic01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


dot_pic :: CtxPicture
dot_pic = drawTracing mf 


mf :: TraceDrawing Double ()
mf = do 
    a <- drawi $ dotCircle `at` zeroPt
    b <- drawi $ dotCircle `at` (P2 60 60)
    _ <- drawi $ dotCircle `at` (P2 120 0)
    p1 <- evalQuery $ radialAnchor (pi/4)    a
    p2 <- evalQuery $ radialAnchor (5* pi/4) b
    draw $ hyperlink link1 $ straightLine p1 p2
  where
    link1 = xlinkhref "http://www.haskell.org"

