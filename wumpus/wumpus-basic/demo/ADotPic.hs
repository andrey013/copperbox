{-# OPTIONS -Wall #-}

module ADotPic where


import Wumpus.Basic.Anchors
import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingMonad

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

-- runFun :: Num u => Drawing u a -> (a, Graphic u)
-- runFun = runDrawing std_attr 

pic1 :: DPicture
pic1 = drawGraphicU $ execDrawing std_attr  $ mf 


mf :: (Floating u, FromPtSize u) => Drawing u ()
mf = do 
    a <- nodeAt dotCircle zeroPt 
    b <- nodeAt dotCircle (P2 60 60)
    _ <- nodeAt dotCircle (P2 45 45)
    let c = radialAnchor (pi/4)  a
    let d = radialAnchor (5* pi/4) b
    trace $ straightLine () (d .-. c) c


