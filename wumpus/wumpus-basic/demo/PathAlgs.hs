{-# OPTIONS -Wall #-}


module PathAlgs where

import Wumpus.Basic.Geometry.Base
import Wumpus.Basic.Geometry.Paths
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour

import System.Directory




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
    drawl (P2 0 0)   $ duplicateH 7 70 $ dcDisk FILL 3
    drawl (P2 0   0) $ distribH 70 [ rect1, rect2, diamond1, poly5
                                    , arc1, arc2, circle1 ]



rect1 :: LocGraphic Double
rect1 = promoteLoc $ \pt -> 
    zapQuery (vertexPP $ runPathAlgPoint pt $ rectanglePathAlg 36 24) 
      >>= dcClosedPath STROKE


rect2 :: LocGraphic Double
rect2 = promoteLoc $ \pt ->
    zapQuery (vertexPP $ runPathAlgPoint pt $ blRectanglePathAlg 36 24) 
      >>= dcClosedPath STROKE

diamond1 :: LocGraphic Double
diamond1 = promoteLoc $ \pt ->
    zapQuery (vertexPP $ runPathAlgPoint pt $ diamondPathAlg 16 20) 
      >>= dcClosedPath STROKE


poly5 :: LocGraphic Double
poly5 = promoteLoc $ \pt ->
    zapQuery (vertexPP $ runPathAlgPoint pt $ polygonPathAlg 5 20) 
      >>= dcClosedPath STROKE

arc1 :: LocGraphic Double
arc1 = promoteLoc $ \pt ->
    zapQuery (curvePP $ runPathAlgPoint pt $ arcPathAlg 20 0 (0.5*pi)) 
      >>= dcOpenPath

arc2 :: LocGraphic Double
arc2 = promoteLoc $ \pt ->
    zapQuery (curvePP $ runPathAlgPoint pt $ arcPathAlg 20 quarter_pi (1.5*pi)) 
      >>= dcOpenPath


circle1 :: LocGraphic Double
circle1 = promoteLoc $ \pt ->
    zapQuery (curvePP $ runPathAlgPoint pt $ circlePathAlg 20) 
      >>= dcClosedPath STROKE



firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6


