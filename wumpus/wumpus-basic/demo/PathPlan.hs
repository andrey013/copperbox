{-# OPTIONS -Wall #-}


module PathPlan where

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
    drawl zeroPt $ renderAnaTrail CFILL_STROKE $ polygonTrail 5 20
    drawl (P2  80 0)   $ renderCatTrail OSTROKE $ triCurve CW 40 50 0
    drawl (P2 160 0)   $ renderCatTrail OSTROKE $ rectCurve CW 40 50 0
    drawl (P2 240 0)   $ renderCatTrail OSTROKE $ bowCurve CW 40 40 0
    drawl (P2 320 0)   $ renderCatTrail OSTROKE $ wedgeCurve CW 40 40 0
    drawl (P2 400 0)   $ renderCatTrail OSTROKE $ loopCurve CW 20 40 0
    drawl (P2 480 0)   $ renderCatTrail OSTROKE $ trapCurve CCW 40 15 (d2r 45) 0 
    drawl (P2  20 80)  $ dcCircle DRAW_FILL 20 
    drawl (P2  40 50)  $ renderCatTrail OSTROKE $ stick1
    drawl (P2 100 80)  $ renderCatTrail OSTROKE $ stick2
    drawl (P2 180 80)  $ renderCatTrail OSTROKE $ stick3
    drawl (P2 260 80)  $ renderCatTrail OSTROKE $ stick4
    drawl (P2 340 80)  $ renderCatTrail OSTROKE $ stick5
    drawl (P2 380 80)  $ renderCatTrail OSTROKE $ stick6
    drawl (P2 440 84)  $ renderCatTrail OSTROKE $ semiellipseTrail CW  10 $ V2 40 5
    drawl (P2 445 80)  $ renderCatTrail OSTROKE $ semiellipseTrail CCW 10 $ V2 40 5
    drawl (P2   0 160) $ renderCatTrail OSTROKE $ sineWave 10 20 0
    drawl (P2 210 160) $ renderCatTrail OSTROKE $ squiggleWave 10 20 0
    drawl (P2   0 200) $ renderCatTrail OSTROKE $ sawtoothWave 10 20 0
    drawl (P2 210 200) $ renderCatTrail OSTROKE $ squareWave 10 20 0
    drawl (P2   0 240) $ renderCatTrail OSTROKE $ semicircleWave CW 10 20 0
    drawl (P2 210 240) $ renderCatTrail OSTROKE $ semicircleWave CCW 10 20 0


stick1 :: CatTrail Double
stick1 = trail_right 25 `mappend` circleSweep CCW (d2r 315.0) 20 0

stick2 :: CatTrail Double
stick2 = trail_right 25 `mappend` circleSweep CW (d2r 315.0) 20 0

stick3 :: CatTrail Double
stick3 = trail_right 15 `mappend` semicircleTrail CW (hvec 30)

stick4 :: CatTrail Double
stick4 = trail_right 15 `mappend` semicircleTrail CCW (hvec 30)

stick5 :: CatTrail Double
stick5 =      trail_up_right 10 
    `mappend` circularArc CW (1.5*pi) 20 quarter_pi 
    `mappend` trail_up_right 10

stick6 :: CatTrail Double
stick6 =      trail_up_right 10 
    `mappend` circularArc CCW half_pi 20 quarter_pi 
    `mappend` trail_up_right 10


black                   :: RGBi
black                   = RGBi 0 0 0

firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6


