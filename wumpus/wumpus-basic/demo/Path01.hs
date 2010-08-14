{-# OPTIONS -Wall #-}

module Path01 where

import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Paths
import Wumpus.Basic.Paths.Base
import Wumpus.Basic.Paths.Construction
import Wumpus.Basic.SVGColours

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace

import System.Directory


main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/path01.eps" pic1
    >> writeSVG_latin1 "./out/path01.svg" pic1 


std_attr :: DrawingAttr
std_attr = standardAttr 19

pic1 :: Picture Double
pic1 = drawGraphicU $ 
           barb90 std_attr 0 (P2 110 0)
         . barb60 std_attr 0 (P2 120 0)
         . barb45 std_attr 0 (P2 130 0)
         . eastUpWest
         . curve1
         . curve2
         . curve3
         . circle1
         
curve1 :: Graphic Double
curve1 = wrapG $ ostroke (strokeAttr std_attr) $ curvedPath xs
  where
    xs = [P2 0 0, P2 32 0, P2 60 28, P2 60 60] 

red_attr :: DrawingAttr
red_attr = std_attr { line_width = 6, stroke_colour = red }

curve2 :: Graphic Double
curve2 = wrapG $ ostroke (strokeAttr red_attr) $ toPathU path1
  where
    path1 = execPath zeroPt $ curveto 0 (3*pi/2) (P2 60 60)


blue_attr :: DrawingAttr
blue_attr = std_attr { line_width = 2, stroke_colour = blue }

curve3 :: Graphic Double
curve3 = wrapG $ ostroke (strokeAttr blue_attr) $ toPathU $ shorten 10 path1

path1 :: BPath Double
path1 = execPath (P2 60 0) $ curveto (pi/2) 0 (P2 0 60)


circle1 :: Graphic Double
circle1 = filledCircle (yellow) 2 60 zeroPt

cto4 :: BPath Double
cto4 = execPath (P2 180 0) $ curveto (pi/2) 0 (P2 120 60)

-- Note - the distance from the barb ends to the curve is not 
-- evenly spaced
-- 
-- Probably have to average the end tangent and the tangent of 
-- the curve \'shortened\' by the arrow tips back distance.
--

eastUpWest :: Graphic Double
eastUpWest = wrapG $ ostroke (strokeAttr blue_attr) $ mkP1 (P2 140 0) (P2 160 20)


-- Potentially this may introduce the style that using AGraphic2 
-- would follow.
--
-- i.e. start and end are supplied, line manipulations are done
-- by displacing them.
--  
mkP1 :: Floating u => Point2 u -> Point2 u -> Path u
mkP1 start end = toPathU $ execPath start $ 
                    horizontalVertical (end .+^ hvec 20) >> lineto end