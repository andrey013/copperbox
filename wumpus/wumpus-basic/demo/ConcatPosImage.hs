{-# OPTIONS -Wall #-}


module ConcatPosImage where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

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
    draw $ testDrawO CENTER `at` (P2   0 475)
    draw $ testDrawO NN     `at` (P2 100 475)
    draw $ testDrawO SS     `at` (P2 200 475)
    draw $ testDrawO EE     `at` (P2 300 475)
    draw $ testDrawO WW     `at` (P2 400 475)
    draw $ testDrawO NE     `at` (P2 100 400)
    draw $ testDrawO SE     `at` (P2 200 400)
    draw $ testDrawO SW     `at` (P2 300 400)
    draw $ testDrawO NW     `at` (P2 400 400)
    draw $ testDrawH CENTER `at` (P2   0 275)
    draw $ testDrawH NN     `at` (P2 100 275)
    draw $ testDrawH SS     `at` (P2 200 275)
    draw $ testDrawH EE     `at` (P2 300 275)
    draw $ testDrawH WW     `at` (P2 400 275)
    draw $ testDrawH NE     `at` (P2 100 200)
    draw $ testDrawH SE     `at` (P2 200 200)
    draw $ testDrawH SW     `at` (P2 300 200)
    draw $ testDrawH NW     `at` (P2 400 200)
    draw $ testDrawV CENTER `at` (P2   0 75)
    draw $ testDrawV NN     `at` (P2 100 75)
    draw $ testDrawV SS     `at` (P2 200 75)
    draw $ testDrawV EE     `at` (P2 300 75)
    draw $ testDrawV WW     `at` (P2 400 75)
    draw $ testDrawV NE     `at` (P2 100 0)
    draw $ testDrawV SE     `at` (P2 200 0)
    draw $ testDrawV SW     `at` (P2 300 0)
    draw $ testDrawV NW     `at` (P2 400 0)
    

testDrawO :: (Floating u, Ord u) => RectPosition -> LocGraphic u
testDrawO rpos = filledDisk 2 `oplus` ignoreAns ans
  where
    ans = let pg = startPosition rpos (rectLong `oplus` rectTall)
          in promoteR1 $ \pt-> 
              (pg `at` pt) >>= \(r,g0) -> drawRect r `oplus` (return (uNil,g0))
          


testDrawH :: (Floating u, Ord u) => RectPosition -> LocGraphic u
testDrawH rpos = filledDisk 2 `oplus` ignoreAns ans
  where
    ans = let pg = startPosition rpos (rectLong `hplus` rectTall)
          in promoteR1 $ \pt-> 
              (pg `at` pt) >>= \(r,g0) -> drawRect r `oplus` (return (uNil,g0))

testDrawV :: (Floating u, Ord u) => RectPosition -> LocGraphic u
testDrawV rpos = filledDisk 2 `oplus` ignoreAns ans
  where
    ans = let pg = startPosition rpos (rectLong `vplus` rectTall)
          in promoteR1 $ \pt-> 
              (pg `at` pt) >>= \(r,g0) -> drawRect r `oplus` (return (uNil,g0))
              
drawRect :: Fractional u => BorderRect u -> Graphic u
drawRect (BorderRect bl w h) = 
    localize (strokeColour blue) $ strokedRectangle w h `at` bl


rectTall :: Floating u => PosGraphic u 
rectTall = makePosGraphic opos (mkRectBl w h)
  where
    w    = 10
    h    = 30
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




rectLong :: Floating u => PosGraphic u 
rectLong = makePosGraphic opos (mkRectMinor m w h)
  where
    m    = 10
    w    = 40 
    h    = 15
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

