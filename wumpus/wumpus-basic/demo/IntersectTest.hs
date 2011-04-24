{-# OPTIONS -Wall #-}


module IntersectTest where

import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import System.Directory




main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let ans = runCtxPictureU std_attr pic01
    writeEPS "./out/intersect_test.eps" ans
    writeSVG "./out/intersect_test.svg" ans


std_attr :: DrawingContext
std_attr = (stroke_colour firebrick . fill_colour black) $ standardContext 12


pic01 :: CtxPicture
pic01 = drawTracing tdrawing


tdrawing :: TraceDrawing Double ()
tdrawing = do
    draw $ illustrateLine line1
    draw $ illustrateLineSegment line_seg1


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


