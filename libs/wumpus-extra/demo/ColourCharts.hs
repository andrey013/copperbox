

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
    writeEPS_latin1 "./out/X11colours.eps" $ uniformScale 0.75 x11_port
    writeSVG_latin1 "./out/X11colours.svg" x11_land
  where
    svg, x11_land, x11_port :: Picture Double
    svg = mkPic all_svg_colours (pointsColwise 59 0 0 4 (scalePt 160))
    x11_land = mkPic all_x11_colours (pointsColwise 59 0 0 5 (scalePt 140))
    x11_port = mkPic all_x11_colours (pointsColwise 71 0 0 4 (scalePt 140))
    
    mkPic cs pts = multi $ zipWith colourSample cs pts

    scalePt w (P2 x y) = P2 (x*w) (y*12) 

colourSample :: (Fractional u, Ord u) 
             => (String,DRGB) -> Point2 u -> Picture u
colourSample (name,c) pt = multi [block, lbl] where
  block = frame $ fillPolygon c $ rectangle 15 10 pt
  lbl   = textline courier10 (pt .+^ hvec 18) name


