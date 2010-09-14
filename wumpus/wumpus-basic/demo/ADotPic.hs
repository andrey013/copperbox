{-# OPTIONS -Wall #-}

module ADotPic where


import Wumpus.Basic.Anchors
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/andots01.eps" pic1
    writeSVG_latin1 "./out/andots01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing std_attr $ mf 


mf :: (Floating u, FromPtSize u) => Drawing u ()
mf = do 
    a <- drawAtImg zeroPt     dotCircle
    b <- drawAtImg (P2 60 60) dotCircle 
    _ <- drawAtImg (P2 45 45) dotCircle 
    let c = radialAnchor (pi/4)  a
    let d = radialAnchor (5* pi/4) b
    drawAt c $ straightLine (d .-. c)


