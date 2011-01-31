{-# OPTIONS -Wall #-}


module ConcatPosImage where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/concat_pos_image01.eps" pic1
    writeSVG "./out/concat_pos_image01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


drawing01 :: DCtxPicture
drawing01 = drawTracing $ localize (fillColour red) $ mf 


mf :: (Floating u, Ord u, FromPtSize u) => TraceDrawing u ()
mf = do
    draw $ testDrawC NN     `at` (P2   0 300)
    draw $ testDrawC SS     `at` (P2  75 300)
    draw $ testDrawC EE     `at` (P2 150 300)
    draw $ testDrawC WW     `at` (P2 225 300)
    draw $ testDrawC NE     `at` (P2   0 225)
    draw $ testDrawC SE     `at` (P2  75 225)
    draw $ testDrawC SW     `at` (P2 150 225)
    draw $ testDrawC NW     `at` (P2 225 225)
    draw $ testDrawC CENTER `at` (P2   0 150)
    

testDrawC :: (Floating u, Ord u) => RectPosition -> LocGraphic u
testDrawC rpos = filledDisk 2 `oplus` ignoreAns ans
  where
    ans = startPosition rpos (rectBl `oplus` rectMinor)

rectBl :: Floating u => PosGraphic u 
rectBl = makePosGraphic opos (mkRectBl w h)
  where
    w    = 15 
    h    = 40
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

