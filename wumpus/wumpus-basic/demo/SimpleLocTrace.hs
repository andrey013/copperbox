{-# OPTIONS -Wall #-}


module SimpleLocTrace where

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
    writeEPS "./out/simple_loc_trace01.eps" pic1
    writeSVG "./out/simple_loc_trace01.svg" pic1


std_attr :: DrawingContext
std_attr = (stroke_colour firebrick . fill_colour black) $ standardContext 12


drawing01 :: CtxPicture
drawing01 = drawTracing mf1


mf1 :: TraceDrawing Double ()
mf1 = do
    drawl (P2 0   0) $ execLocTrace $
         write rect1    >> hmoveBy 30 
      >> write rect2    >> hmoveBy 30
      >> write diamond1 >> vmoveBy 50 
      >> write (localize (stroke_colour indigo) poly5)   -- no move
      >> write circle1



rect1 :: LocGraphic Double
rect1 = promoteR1 $ \pt -> 
    vertexPP (runPathAlgPoint pt $ rectanglePathAlg 20 10) >>= dcClosedPath STROKE


rect2 :: LocGraphic Double
rect2 = promoteR1 $ \pt ->
    vertexPP (runPathAlgPoint pt $ blRectanglePathAlg 10 20) >>= dcClosedPath STROKE

diamond1 :: LocGraphic Double
diamond1 = promoteR1 $ \pt ->
    vertexPP (runPathAlgPoint pt $ diamondPathAlg 5 10) >>= dcClosedPath STROKE


poly5 :: LocGraphic Double
poly5 = promoteR1 $ \pt ->
    vertexPP (runPathAlgPoint pt $ polygonPathAlg 5 22) >>= dcClosedPath STROKE


circle1 :: LocGraphic Double
circle1 = promoteR1 $ \pt ->
    curvePP (runPathAlgPoint pt $ circlePathAlg 10) >>= dcClosedPath STROKE



firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6


indigo                  :: RGBi
indigo                  = RGBi 75 0 130
