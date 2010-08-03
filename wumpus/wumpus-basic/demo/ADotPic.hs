{-# OPTIONS -Wall #-}

module ADotPic where


import Wumpus.Basic.Anchors
import Wumpus.Basic.AnchorDots
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.STraceMonad

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space
import MonadLib                         -- package: monadLib

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

runFun :: STraceT (Primitive u) Id a -> (a, Graphic u)
runFun = runId . runSTraceT

pic1 :: DPicture
pic1 = drawGraphicU $ snd $ runFun  $ mf 


mf :: STraceT DPrimitive Id ()
mf = do 
    a <- dotCircle std_attr zeroPt 
    b <- dotCircle std_attr (P2 60 60)
    let c = radialAnchor (pi/4)  a
    let d = radialAnchor (5* pi/4) b
    strace $ straightLine () (d .-. c) c


