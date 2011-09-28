{-# OPTIONS -Wall #-}


module PathPlan where

import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid
import System.Directory




main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let ans = runCtxPictureU std_attr pic01
    writeEPS "./out/path_plan.eps" ans
    writeSVG "./out/path_plan.svg" ans


std_attr :: DrawingContext
std_attr = (stroke_colour firebrick . fill_colour lemon_chiffon) $ standardContext 12


pic01 :: CtxPicture
pic01 = drawTracing tdrawing


tdrawing :: TraceDrawing Double ()
tdrawing = do
    drawl zeroPt $ drawAnaTrail CFILL_STROKE $ polygonTrail 5 20
    drawl (P2  80 0)   $ drawCatTrail OSTROKE $ tricurve CW 40 50 0
    drawl (P2 160 0)   $ drawCatTrail OSTROKE $ rectcurve CW 40 50 0
    drawl (P2 240 0)   $ drawCatTrail OSTROKE $ bowcurve CW 40 40 0
    drawl (P2 320 0)   $ drawCatTrail OSTROKE $ wedgecurve CW 40 40 0
    drawl (P2 400 0)   $ drawCatTrail OSTROKE $ loopcurve CW 20 40 0
    drawl (P2 480 0)   $ drawCatTrail OSTROKE $ trapcurve CCW 40 15 (d2r 45) 0 
    drawl (P2  20 80)  $ dcCircle DRAW_FILL 20 
    drawl (P2  40 50)  $ drawCatTrail OSTROKE $ stick1
    drawl (P2 100 80)  $ drawCatTrail OSTROKE $ stick2
    drawl (P2 180 80)  $ drawCatTrail OSTROKE $ stick3
    drawl (P2 260 80)  $ drawCatTrail OSTROKE $ stick4
    drawl (P2 340 80)  $ drawCatTrail OSTROKE $ stick5
    drawl (P2 380 80)  $ drawCatTrail OSTROKE $ stick6
    drawl (P2   0 160) $ drawCatTrail OSTROKE $ sineWave 10 20 0
    drawl (P2 210 160) $ drawCatTrail OSTROKE $ squiggleWave 10 20 0
    drawl (P2   0 200) $ drawCatTrail OSTROKE $ sawtoothWave 10 20 0
    drawl (P2 210 200) $ drawCatTrail OSTROKE $ squareWave 10 20 0
    drawl (P2   0 240) $ drawCatTrail OSTROKE $ semicircleWave CW 10 20 0
    drawl (P2 210 240) $ drawCatTrail OSTROKE $ semicircleWave CCW 10 20 0


stick1 :: CatTrail Double
stick1 = trail_right 25 `mappend` circleSweepCCW (d2r 315.0) 20 0

stick2 :: CatTrail Double
stick2 = trail_right 25 `mappend` circleSweepCW (d2r 315.0) 20 0

stick3 :: CatTrail Double
stick3 = trail_right 15 `mappend` semicircleCW (hvec 30)

stick4 :: CatTrail Double
stick4 = trail_right 15 `mappend` semicircleCCW (hvec 30)

stick5 :: CatTrail Double
stick5 =      trail_up_right 10 
    `mappend` circularArcCW (1.5*pi) 20 quarter_pi 
    `mappend` trail_up_right 10

stick6 :: CatTrail Double
stick6 =      trail_up_right 10 
    `mappend` circularArcCCW half_pi 20 quarter_pi 
    `mappend` trail_up_right 10



test1 :: Maybe DPoint2
test1 = interLinesegLine line_seg1 line1



line_seg1 :: LineSegment Double
line_seg1 = LineSegment (P2 (-150) (-63)) (P2 150 (-63))

line1 :: Line Double
line1 = Line zeroPt (P2 34 (-93))


black                   :: RGBi
black                   = RGBi 0 0 0

firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6


