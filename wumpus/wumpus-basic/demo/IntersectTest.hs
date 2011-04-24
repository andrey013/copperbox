{-# OPTIONS -Wall #-}


module IntersectTest where

import Wumpus.Basic.Geometry
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import System.Directory

dummy1 :: Maybe DPoint2
dummy1 = interLinesegLine lseg1 line1
  where
    lseg1 = LineSegment (P2 (-150) (-63)) (P2 150 (-63))
    line1 = Line zeroPt (P2 34 (-93))




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

test2 :: Maybe DPoint2
test2 = interLineLine (Line p1 p2) line1
  where
   LineSegment p1 p2 = line_seg1

test3 :: Maybe Bool
test3 = fmap (\a -> withinPoints a line_seg1) test2


withinPoints :: (Ord u, Fractional u, Tolerance u) 
             => Point2 u -> LineSegment u -> Bool
withinPoints (P2 x y) (LineSegment (P2 x0 y0) (P2 x1 y1)) =  
    between x (ordpair x0 x1) && between y (ordpair y0 y1)
  where
    ordpair a b     = (min a b, max a b)
    between a (s,t) = (s `tGTE` a) && (a `tLTE` t)


fZ :: (Ord u, Fractional u, Tolerance u) 
             => LineSegment u -> ((u,u), (u,u))
fZ (LineSegment (P2 x0 y0) (P2 x1 y1)) = (ordpair x0 x1,ordpair y0 y1)
  where
    ordpair a b     = (min a b, max a b)
    between a (s,t) = (s `tLTE` a) && (a `tLTE` t)




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


