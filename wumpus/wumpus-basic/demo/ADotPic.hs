{-# OPTIONS -Wall #-}

module ADotPic where


import Wumpus.Basic.Anchors
import Wumpus.Basic.AnchorDots
import qualified Wumpus.Basic.Dots as BD
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.STraceMonad
import Wumpus.Basic.SVGColours

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


std_attr :: BD.MarkAttr
std_attr = BD.standardAttr 24

runFun :: STraceT (Primitive u) Id a -> (a, Graphic u)
runFun = runId . runSTraceT

pic1 :: DPicture
pic1 = drawGraphicU $ snd $ runFun  $ mf 


mf :: STraceT DPrimitive Id ()
mf = do 
    a <- dotCircle std_attr zeroPt 
    b <- dotCircle std_attr (P2 60 0)
    let c = radialAnchor 0  a
    let d = radialAnchor pi b
    strace $ straightLine () (d .-. c) c


