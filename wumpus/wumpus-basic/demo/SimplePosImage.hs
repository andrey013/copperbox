{-# OPTIONS -Wall #-}


module SimplePosImage where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/pos_image01.eps" pic1
    writeSVG "./out/pos_image01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


drawing01 :: DCtxPicture
drawing01 = drawTracing $ localize (fillColour red) $ mf 


mf :: (Floating u, FromPtSize u) => TraceDrawing u ()
mf = do
    draw $ testDraw NN `at` (P2   0 200)
    draw $ testDraw SS `at` (P2  75 200)
    draw $ testDraw EE `at` (P2 150 200)
    draw $ testDraw WW `at` (P2 225 200)
    draw $ testDraw NE `at` (P2   0 100)
    draw $ testDraw SE `at` (P2  75 100)
    draw $ testDraw SW `at` (P2 150 100)
    draw $ testDraw NW `at` (P2 225 100)
    draw $ testDraw CENTER    `at` (P2   0 0)
    draw $ testDraw BL_LEFT   `at` (P2  75 0)
    draw $ testDraw BL_CENTER `at` (P2 150 0)
    draw $ testDraw BL_RIGHT  `at` (P2 225 0)
    

testDraw :: Floating u => RectPosition -> LocGraphic u
testDraw rpos = filledDisk 2 `oplus` ans
  where
    ans = setPosition rpos anglePosG `rot` (pi * 0.25)

anglePosG :: Floating u => PosGraphic u 
anglePosG = makePosImage opos (angRect w h)
  where
    w    = 40 
    h    = 20
    opos = ObjectPos { op_x_minor = 0
                     , op_x_major = w
                     , op_y_minor = 0
                     , op_y_major = h }
 

-- start-point - bottom left
angRect :: Floating u => u -> u -> LocThetaGraphic u
angRect w h = promoteR2 $ \bl theta -> 
    let br = displaceParallel w theta bl
        tr = displacePerpendicular h theta br
        tl = displacePerpendicular h theta bl
    in closedStroke $ vertexPath [bl, br, tr, tl]

