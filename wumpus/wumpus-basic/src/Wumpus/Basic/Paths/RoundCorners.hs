{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths.RoundCorners
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Drawing round cornered polygons.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Paths.RoundCorners
  ( 
    cornerCurve
  , illustratePath
  , roundEvery

  ) where

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.Paths.Base hiding ( length )
import Wumpus.Basic.Utils.Intersection


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Data.Monoid


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


roundEvery :: (Real u, Floating u) 
           => u -> [Point2 u] -> Path u 
roundEvery u (start:b:c:xs) = step (twoParts u start b c) (b:c:xs)
  where
    step acc (m:n:o:ps)     = step (acc `mappend` twoParts u m n o) (n:o:ps)
    step acc [n,o]          = acc `mappend` twoParts u n o start
                                  `mappend` twoParts u o start b 
    step acc _              = acc
roundEvery _ _              = mempty


-- | Two parts - line and corner curve...
--
twoParts :: (Real u, Floating u) 
         => u ->  Point2 u -> Point2 u -> Point2 u -> Path u
twoParts u a b c = line p1 p2 `mappend` cornerCurve p2 b p3
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
illustratePath = localDF (primaryColour black) . step1 . pathViewL
  where
    step1 EmptyPathL = mempty
    step1 (e :<< se) = drawPathBoth e `mappend` rest (pathViewL se)

    rest EmptyPathL = mempty
    rest (e :<< se) = drawPath1 e `mappend` rest (pathViewL se)

drawPathBoth :: Fractional u => PathSegment u -> Graphic u
drawPathBoth pa@(Line1 p1 _)      = drawPath1 pa `mappend` pathPoint p1
drawPathBoth pa@(Curve1 p1 _ _ _) = drawPath1 pa `mappend` pathPoint p1

drawPath1 :: Fractional u => PathSegment u -> Graphic u
drawPath1 (Line1 p1 p2)        = 
    straightLineBetween p1 p2 `mappend` pathPoint p2

drawPath1 (Curve1 p1 p2 p3 p4) =  
    mconcat [ bezierCtrl p1 p2, bezierCtrl p4 p3, curveBetween p1 p2 p3 p4
            , pathPoint p4]


bezierCtrl :: Fractional u => Point2 u -> Point2 u -> Graphic u
bezierCtrl p1 p2 = 
    localDF (primaryColour light_steel_blue . secondaryColour red) $
      straightLineBetween p1 p2 `mappend` filledDisk 1 p2


pathPoint :: Num u => Point2 u -> Graphic u
pathPoint = localDF bothPrimary . filledDisk 1

