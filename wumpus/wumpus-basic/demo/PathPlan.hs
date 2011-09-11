{-# OPTIONS -Wall #-}


module PathPlan where

import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

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
    drawl zeroPt $ drawPlacedTrail CFILL_STROKE $ polygonTrail 5 20
    drawl (P2   0 160) $ drawCatTrail OSTROKE $ sineWave 10 20 0
    drawl (P2 210 160) $ drawCatTrail OSTROKE $ squiggleWave 10 20 0
    drawl (P2   0 200) $ drawCatTrail OSTROKE $ sawtoothWave 10 20 0
    drawl (P2 210 200) $ drawCatTrail OSTROKE $ squareWave 10 20 0

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


