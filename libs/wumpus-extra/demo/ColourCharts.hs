

module ColourCharts where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Geometry

import Wumpus.Extra.SVGColourChart
import Wumpus.Extra.X11ColourChart

import Data.AffineSpace

import Data.List

main :: IO ()
main = sequence_ [ test01 ]

-- This is wrong -- can't do one scatter plot over another as
-- each one is at liberty to change the /scaling/. This needs a 
-- rethink...
test01 :: IO ()
test01 = do 
    writeEPS_latin1 "./out/SVGcolours.eps" svg
    writeSVG_latin1 "./out/SVGcolours.svg" svg
    writeEPS_latin1 "./out/X11colours.eps" $ uniformScale 0.75 x11
    writeSVG_latin1 "./out/X11colours.svg" x11
  where
    svg, x11 :: Picture Double
    svg = mkPic all_svg_colours 160 
    x11 = mkPic all_x11_colours 140
    
    mkPic cs w = multi $ zipWith colourSample cs (points w)

colourSample :: (Fractional u, Ord u) 
             => (String,DRGB) -> Point2 u -> Picture u
colourSample (name,c) pt = multi [block, lbl] where
  block = frame $ fillPolygon c $ rectangle 15 10 pt
  lbl   = textline courier10 (pt .+^ hvec 18) name


-- infinite
points :: (Num u, Ord u) => u -> [Point2 u]
points w = [P2 (x*w) (y*12) | x <- iterate (+1) 0, y <- countdown 59 ]

countdown :: (Num u, Ord u) => u -> [u]
countdown = unfoldr phi where
   phi i | i < 0 = Nothing
   phi i         = Just (i,i-1)