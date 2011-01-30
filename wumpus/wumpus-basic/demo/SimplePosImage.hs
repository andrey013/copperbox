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
    draw $ testDrawMinor NN     `at` (P2   0 300)
    draw $ testDrawMinor SS     `at` (P2  75 300)
    draw $ testDrawMinor EE     `at` (P2 150 300)
    draw $ testDrawMinor WW     `at` (P2 225 300)
    draw $ testDrawMinor NE     `at` (P2   0 225)
    draw $ testDrawMinor SE     `at` (P2  75 225)
    draw $ testDrawMinor SW     `at` (P2 150 225)
    draw $ testDrawMinor NW     `at` (P2 225 225)
    draw $ testDrawMinor CENTER `at` (P2   0 150)
    draw $ testDrawBl    CENTER `at` (P2 225 150)
    draw $ testDrawBl    NN     `at` (P2   0 75)
    draw $ testDrawBl    SS     `at` (P2  75 75)
    draw $ testDrawBl    EE     `at` (P2 150 75)
    draw $ testDrawBl    WW     `at` (P2 225 75)
    draw $ testDrawBl    NE     `at` (P2   0 0)
    draw $ testDrawBl    SE     `at` (P2  75 0)
    draw $ testDrawBl    SW     `at` (P2 150 0)
    draw $ testDrawBl    NW     `at` (P2 225 0)
    

testDrawBl :: Floating u => RectPosition -> LocGraphic u
testDrawBl rpos = filledDisk 2 `oplus` ignoreAns ans
  where
    ans = setPosition rpos rectBl

rectBl :: Floating u => PosGraphic u 
rectBl = makePosGraphic opos (mkRectBl w h)
  where
    w    = 40 
    h    = 20
    opos = ObjectPos { op_x_minor = 0
                     , op_x_major = w
                     , op_y_minor = 0
                     , op_y_major = h }
 

-- start-point - bottom left
mkRectBl :: Floating u => u -> u -> LocGraphic u
mkRectBl w h = promoteR1 $ \bl -> 
    let br = displaceH w bl
        tr = displaceV h br
        tl = displaceV h bl
    in closedStroke $ vertexPath [bl, br, tr, tl]



testDrawMinor :: Floating u => RectPosition -> LocGraphic u
testDrawMinor rpos = filledDisk 2 `oplus` ignoreAns ans
  where
    ans = setPosition rpos rectMinor

rectMinor :: Floating u => PosGraphic u 
rectMinor = makePosGraphic opos (mkRectMinor m w h)
  where
    m    = 10
    w    = 40 
    h    = 20
    opos = ObjectPos { op_x_minor = m
                     , op_x_major = (w-m)
                     , op_y_minor = m
                     , op_y_major = (h-m) }
 

-- start-point - +10 +10
mkRectMinor :: Floating u => u -> u -> u -> LocGraphic u
mkRectMinor m w h = promoteR1 $ \pt -> 
    let bl = displaceVec (vec (-m) (-m)) pt
        br = displaceH w bl
        tr = displaceV h br
        tl = displaceV h bl
    in closedStroke $ vertexPath [bl, br, tr, tl]

