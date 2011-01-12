{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.RoundCorners
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Drawing round cornered polygons.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.RoundCorners
  ( 
    cornerCurve
  , illustratePath
  , roundEvery

  ) where

import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Geometry.Intersection
import Wumpus.Drawing.Paths.Base hiding ( length )

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


-- | The length of the control-point vector wants to be slighly 
-- longer than half of /d/ (d - being the distance between the 
-- /truncated/ points and the corner).
--
cornerCurve :: (Real u, Floating u) 
            => Point2 u -> Point2 u -> Point2 u -> Path u
cornerCurve p1 p2 p3 = curve p1 cp1 cp2 p3
  where
    len1 = 0.6 *  (vlength $ pvec p1 p2)
    len2 = 0.6 *  (vlength $ pvec p3 p2)
    cp1  = p1 .+^ (avec (langle p1 p2) len1)
    cp2  = p3 .+^ (avec (langle p3 p2) len2)


-- | 'roundEvery' throws a runtime error if the input list has
-- less than 3 elements.
--
roundEvery :: (Real u, Floating u) 
           => u -> [Point2 u] -> Path u 
roundEvery u (start:b:c:xs) = step (twoParts u start b c) (b:c:xs)
  where
    step acc (m:n:o:ps)     = step (acc `append` twoParts u m n o) (n:o:ps)
    step acc [n,o]          = acc `append` twoParts u n o start
                                  `append` twoParts u o start b 
    step acc _              = acc

roundEvery _ _              = error "roundEvery - input list too short."


-- | Two parts - line and corner curve...
--
twoParts :: (Real u, Floating u) 
         => u ->  Point2 u -> Point2 u -> Point2 u -> Path u
twoParts u a b c = line p1 p2 `append` cornerCurve p2 b p3
  where
    p1 = a .+^ (avec (direction $ pvec a b) u)
    p2 = b .+^ (avec (direction $ pvec b a) u)
    p3 = b .+^ (avec (direction $ pvec b c) u)
 


--------------------------------------------------------------------------------
-- 

-- This needs moving outside of the Path modules as it has 
-- dependencies on SVGColour and Graphic (the Path modules should
-- be /neutral/ to wards depenedencies on other parts of 
-- Wumpus-Basic).
--

illustratePath :: Fractional u => Path u -> Graphic u
illustratePath = localize (strokeColour black) . step1 . pathViewL
  where
    step1 (PathOneL e)  = drawPath1 e
    step1 (e :<< se)    = drawPathBoth e `oplus` rest (pathViewL se)

    rest (PathOneL e)   = drawPath1 e
    rest (e :<< se)     = drawPath1 e `oplus` rest (pathViewL se)

drawPathBoth :: Fractional u => PathSegment u -> Graphic u
drawPathBoth pa@(Line1 p1 _)      = drawPath1 pa `oplus` pathPoint p1
drawPathBoth pa@(Curve1 p1 _ _ _) = drawPath1 pa `oplus` pathPoint p1




drawPath1 :: Fractional u => PathSegment u -> Graphic u
drawPath1 (Line1 p1 p2)        = 
    straightLineBetween p1 p2 `oplus` pathPoint p2

drawPath1 (Curve1 p1 p2 p3 p4) =  
    oconcat (bezierCtrl p1 p2) [ bezierCtrl p4 p3, curveBetween p1 p2 p3 p4
                               , pathPoint p4 ]


-- WARNING - This indicates that straightLineBetween is not 
-- consistent with other prim graphics...

bezierCtrl :: Fractional u => Point2 u -> Point2 u -> Graphic u
bezierCtrl p1 p2 = localize (strokeColour light_steel_blue . fillColour red) $
      straightLineBetween p1 p2 `oplus` (filledDisk 1 `at` p2)


pathPoint :: Num u => Point2 u -> Graphic u
pathPoint pt = localize bothStrokeColour (filledDisk 1 `at` pt)

