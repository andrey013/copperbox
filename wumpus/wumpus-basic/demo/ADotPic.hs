{-# OPTIONS -Wall #-}

module ADotPic where


import Wumpus.Basic.Anchors
import Wumpus.Basic.AnchorDots
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.ConsDrawing

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


std_attr :: DrawingAttr
std_attr = standardAttr 24

runFun :: Num u => ConsDrawing u a -> (a, Graphic u)
runFun = runConsDrawing (regularConfig 20) (0,0) std_attr 

pic1 :: DPicture
pic1 = drawGraphicU $ snd $ runFun  $ mf 


mf :: (Floating u, FromPtSize u) => ConsDrawing u ()
mf = do 
    a <- node $ dotCircle `at` zeroPt 
    b <- node $ dotCircle `at` (P2 60 60)
    _ <- node $ dotCircle `at` (P2 45 45)
    let c = radialAnchor (pi/4)  a
    let d = radialAnchor (5* pi/4) b
    trace $ straightLine () (d .-. c) c


