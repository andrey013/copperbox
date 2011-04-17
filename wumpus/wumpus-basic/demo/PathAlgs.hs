{-# OPTIONS -Wall #-}


module PathAlgs where

import Wumpus.Basic.Geometry.Paths
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour

import System.Directory

import Data.VectorSpace

dummy01 :: Vec2 Double
dummy01 = (V2 1 0) ^-^ V2 0 (-1)


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/path_algs01.eps" pic1
    writeSVG "./out/path_algs01.svg" pic1


std_attr :: DrawingContext
std_attr = (stroke_colour firebrick . fill_colour black) $ standardContext 12


drawing01 :: CtxPicture
drawing01 = drawTracing mf1


mf1 :: TraceDrawing Double ()
mf1 = do
    drawl (P2 0 0)   $ duplicateH 6 100 $ filledDisk 3
    drawl (P2 0   0) $ distribH 100 [ rect1, rect2, diamond1, poly5
                                    , wedge1, circle1 ]



rect1 :: LocGraphic Double
rect1 = promoteR1 $ \pt -> 
    vertexPP (runPathAlgPoint pt $ rectanglePathAlg 36 24) >>= closedStroke


rect2 :: LocGraphic Double
rect2 = promoteR1 $ \pt ->
    vertexPP (runPathAlgPoint pt $ blRectanglePathAlg 36 24) >>= closedStroke

diamond1 :: LocGraphic Double
diamond1 = promoteR1 $ \pt ->
    vertexPP (runPathAlgPoint pt $ diamondPathAlg 16 20) >>= closedStroke


poly5 :: LocGraphic Double
poly5 = promoteR1 $ \pt ->
    vertexPP (runPathAlgPoint pt $ polygonPathAlg 5 20) >>= closedStroke

wedge1 :: LocGraphic Double
wedge1 = promoteR1 $ \pt ->
    curvePP (runPathAlgPoint pt $ minorArcPathAlg 20 0 (0.5*pi)) >>= openStroke

circle1 :: LocGraphic Double
circle1 = promoteR1 $ \pt ->
    curvePP (runPathAlgPoint pt $ circlePathAlg 20) >>= closedStroke



firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6


