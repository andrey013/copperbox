{-# OPTIONS -Wall #-}


module QuadrantTest where

import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let ans = runCtxPictureU std_attr pic01
    writeEPS "./out/quadrant_test.eps" ans
    writeSVG "./out/quadrant_test.svg" ans


std_attr :: DrawingContext
std_attr = (stroke_colour firebrick . fill_colour black) $ standardContext 12


pic01 :: CtxPicture
pic01 = drawTracing tdrawing


tdrawing :: TraceDrawing Double ()
tdrawing = do
    drawl zeroPt $ rect1
    drawl zeroPt $ dcDisk FILL 3
    drawl zeroPt $ diskV (runQuadrantAlg (d2r 350) rectQA)
    drawl (P2 100 0) $ dia1
    drawl (P2 100 0) $ dcDisk FILL 3
    drawl (P2 100 0) $ diskV (runQuadrantAlg (d2r 350) diaQA)
    drawl (P2 200 0) $ tri1
    drawl (P2 200 0) $ dcDisk FILL 3
    drawl (P2 200 0) $ diskV (runQuadrantAlg (d2r 350) triQA)
    drawl (P2 300 0) $ quadr1
    drawl (P2 300 0) $ dcDisk FILL 3
    drawl (P2 300 0) $ diskV (qdQI (d2r 10))


quadr1 :: LocGraphic Double
quadr1 = drawVertexPathAlg STROKE (pathStartIsStart vs)
  where
    vs = [ hvec 40 , vec 20 20 , hvec (-60), vvec (-20) ]

qdQI :: RadialIntersect Double
qdQI = qdltrlHMajorQI 60 20 quarter_pi 


rect1 :: LocGraphic Double
rect1 = drawVertexPathAlg STROKE (rectanglePathAlg 80 40)

rectQA :: QuadrantAlg Double
rectQA = rectangleQuadrantAlg 80 40


dia1 :: LocGraphic Double
dia1 = drawVertexPathAlg STROKE (diamondPathAlg 30 40)

diaQA :: QuadrantAlg Double
diaQA = diamondQuadrantAlg 60 80

tri1 :: LocGraphic Double
tri1 = drawVertexPathAlg STROKE (isoscelesTriPathAlg 40 60)

triQA :: QuadrantAlg Double
triQA = isoscelesTriQuadrantAlg 40 60


diskV :: Vec2 Double -> LocGraphic Double
diskV v1 = moveStart (displaceVec v1) $ dcDisk FILL 3


black                   :: RGBi
black                   = RGBi 0 0 0

firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6


