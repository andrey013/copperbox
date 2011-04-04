{-# OPTIONS -Wall #-}


module SimplePosObject where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx drawing01
    writeEPS "./out/simple_pos_object01.eps" pic1
    writeSVG "./out/simple_pos_object01.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


mf :: TraceDrawing Double ()
mf = do
    draw $ testDrawMinor NN     `at` (P2   0 400)
    draw $ testDrawMinor SS     `at` (P2  75 400)
    draw $ testDrawMinor EE     `at` (P2 150 400)
    draw $ testDrawMinor WW     `at` (P2 225 400)
    draw $ testDrawMinor NE     `at` (P2   0 325)
    draw $ testDrawMinor SE     `at` (P2  75 325)
    draw $ testDrawMinor SW     `at` (P2 150 325)
    draw $ testDrawMinor NW     `at` (P2 225 325)
    draw $ testDrawMinor CENTER `at` (P2   0 250)
    draw $ testDrawMinor BLL    `at` (P2  75 250)
    draw $ testDrawMinor BLC    `at` (P2 150 250)
    draw $ testDrawMinor BLR    `at` (P2 225 250)


    draw $ testDrawBl    NN     `at` (P2   0 150)
    draw $ testDrawBl    SS     `at` (P2  75 150)
    draw $ testDrawBl    EE     `at` (P2 150 150)
    draw $ testDrawBl    WW     `at` (P2 225 150)
    draw $ testDrawBl    NE     `at` (P2   0 75)
    draw $ testDrawBl    SE     `at` (P2  75 75)
    draw $ testDrawBl    SW     `at` (P2 150 75)
    draw $ testDrawBl    NW     `at` (P2 225 75)
    draw $ testDrawBl    CENTER `at` (P2   0  0)
    draw $ testDrawBl    BLL    `at` (P2  75  0)
    draw $ testDrawBl    BLC    `at` (P2 150  0)
    draw $ testDrawBl    BLR    `at` (P2 225  0)
    

testDrawBl :: (Floating u, InterpretUnit u) => RectAddress -> LocGraphic u
testDrawBl rpos = filledDisk 2 `oplus` (ignoreAns $ rectBl `startAddr` rpos)

rectBl :: (Fractional u, InterpretUnit u) => BoundedLocRectGraphic u
rectBl = makeBoundedLocRectGraphic $ makePosObject (return ortt) (mkRectBl w h)
  where
    w    = 40 
    h    = 30
    ortt = Orientation { or_x_minor = 0
                       , or_x_major = w
                       , or_y_minor = 0
                       , or_y_major = h }
 

-- start-point - bottom left
mkRectBl :: InterpretUnit u => u -> u -> LocGraphic u
mkRectBl w h = strokedRectangle w h



testDrawMinor :: (Floating u, InterpretUnit u) => RectAddress -> LocGraphic u
testDrawMinor rpos = filledDisk 2 `oplus` (ignoreAns $ rectMinor `startAddr` rpos)

rectMinor :: (Fractional u, InterpretUnit u) => BoundedLocRectGraphic u 
rectMinor = 
    makeBoundedLocRectGraphic $ makePosObject (return ortt) (mkRectMinor m w h)
  where
    m    = 10
    w    = 40 
    h    = 30
    ortt = Orientation { or_x_minor = m
                       , or_x_major = (w-m)
                       , or_y_minor = m
                       , or_y_major = (h-m) }
 

-- start-point - +10 +10
mkRectMinor :: InterpretUnit u => u -> u -> u -> LocGraphic u
mkRectMinor m w h = promoteR1 $ \pt -> 
    let bl = displaceVec (vec (-m) (-m)) pt
        br = displaceH w bl
        tr = displaceV h br
        tl = displaceV h bl
    in vertexPP [bl, br, tr, tl] >>= closedStroke

