{-# OPTIONS -Wall #-}


module QuadrantTest where

import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

dummy1 :: Maybe DPoint2
dummy1 = interLinesegLine lseg1 line1
  where
    lseg1 = LineSegment (P2 (-150) (-63)) (P2 150 (-63))
    line1 = Line zeroPt (P2 34 (-93))


dummy2 :: Maybe DPoint2
dummy2 = interLinesegLine lseg1 line1
  where
    lseg1 = LineSegment (P2 (-150) 0) (P2 150 0)
    line1 = Line (P2 0 63) (P2 34 (-30))


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
    drawl zeroPt $ diskV (runQuadrantAlg (dd2r 350) rectQA)
    drawl (P2 100 0) $ dia1
    drawl (P2 100 0) $ dcDisk FILL 3
    drawl (P2 100 0) $ diskV (runQuadrantAlg (dd2r 350) diaQA)
    drawl (P2 200 0) $ tri1
    drawl (P2 200 0) $ dcDisk FILL 3
    drawl (P2 200 0) $ diskV (runQuadrantAlg (dd2r 350) triQA)
    drawl (P2 300 0) $ quadr1
    drawl (P2 300 0) $ dcDisk FILL 3
    drawl (P2 300 0) $ diskV (qdQI (dd2r 10))

    drawl (P2 0 120) $ para1
    drawl (P2 0 120) $ dcDisk FILL 3
    drawl (P2 0 120) $ diskV (runQuadrantAlg (dd2r 5) paraQA)


dd2r :: Double -> Radian
dd2r = d2r

quadr1 :: LocGraphic Double
quadr1 = drawVertexPathAlg STROKE (pathStartIsStart vs)
  where
    vs = [ hvec 60 , vec (-20) 20 , hvec (-40), vvec (-20) ]

qdQI :: RadialIntersect Double
qdQI = hquadrilAcuteQI 40 20 (0.75 * pi)


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


para1 :: LocGraphic Double 
para1 = drawVertexPathAlg STROKE (parallelogramPathAlg 60 40 (dd2r 45))


paraQA :: QuadrantAlg Double
paraQA = parallelogramQuadrantAlg 60 40 (dd2r 45)


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


