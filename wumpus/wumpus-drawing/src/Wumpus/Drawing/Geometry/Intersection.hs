{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Geometry.Intersection
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Intersection of line to line and line to plane.
-- 
-- \*\* - WARNING \*\* - this uses quite a high tolerance for 
-- floating point equality. 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Geometry.Intersection
  ( 

    LineSegment
  , interLineLine
  , interLinesegLineseg
  , interLinesegLine

  , findIntersect
  , makePlane
  , rectangleLineSegments
  , polygonLineSegments


  ) 
  where

import Wumpus.Drawing.Geometry.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


type LineSegment u = (Point2 u, Point2 u)

-- | 'interLineLine' : @ line1 * line2 -> Maybe Point @
-- 
-- Find the intersection of two lines, if there is one. 
--
-- Lines are infinite they are represented by points on them, 
-- they are not line segments.
--
-- An answer of @Nothing@ may indicate wither the lines coincide
-- or the are parallel.
--
interLineLine :: Fractional u 
              => (Point2 u, Point2 u) -> (Point2 u, Point2 u) 
              -> Maybe (Point2 u)
interLineLine (p1,p2) (q1,q2) = 
    if det_co == 0 then Nothing 
                   else Just $ P2 (det_xm / det_co) (det_ym / det_co)
  where
    -- Ax + By + C = 0
    LineEquation a1 b1 c1 = lineEquation p1 p2
    LineEquation a2 b2 c2 = lineEquation q1 q2

    coeffM                = M2'2 a1 b1  a2 b2
    det_co                = det2'2 coeffM

    xM                    = M2'2  (negate c1) b1  (negate c2) b2
    det_xm                = det2'2 xM

    yM                    = M2'2  a1 (negate c1) a2 (negate c2)
    det_ym                = det2'2 yM



-- | 'interLinesegLineseg' : @ line_segment1 * line_segment2 -> Maybe Point @
-- 
-- Find the intersection of two line segments, if there is one. 
--
-- An answer of @Nothing@ indicates that the line segments 
-- coincide, or that there is no intersection.
--
interLinesegLineseg :: (Fractional u, Ord u, FromPtSize u)
                    => LineSegment u -> LineSegment u -> Maybe (Point2 u)
interLinesegLineseg l1 l2 = interLineLine l1 l2 >>= segcheck
  where
    segcheck = mbCheck (\pt -> withinPoints pt l1 && withinPoints pt l2)
 

-- | 'interLinesegLine' : @ line_segment * line -> Maybe Point @
-- 
-- Find the intersection of a line and a line segment, if there 
-- is one. 
--
-- An answer of @Nothing@ indicates that the the line and line
-- segment coincide, or that there is no intersection.
--
interLinesegLine :: (Fractional u, Ord u, FromPtSize u)
                 => LineSegment u -> (Point2 u, Point2 u) -> Maybe (Point2 u)
interLinesegLine seg line = interLineLine seg line >>= segcheck
  where
    segcheck = mbCheck (\pt -> withinPoints pt seg)


mbCheck :: (a -> Bool) -> a -> Maybe a
mbCheck test a = if test a then Just a else Nothing

-- | Check the point is \"within\" the span of the line.
--
-- Note - this function is to be used \*after\* an intersection
-- has been found. Hence it is not export.
--
withinPoints :: (Ord u, FromPtSize u) => Point2 u -> (Point2 u, Point2 u) -> Bool
withinPoints (P2 x y) (P2 x0 y0, P2 x1 y1) =  
    between x (ordpair x0 x1) && between y (ordpair y0 y1)
  where
    ordpair a b     = (min a b, max a b)
    between a (s,t) = (s `tGT` a) && (a `tGT` t)

    tGT a b         = a < b || abs (a-b) < tolerance

-- | Note - its important to use tolerance for the @withPoints@ 
-- function.
--
tolerance :: FromPtSize u => u
tolerance = fromPtSize 0.01


-- | 'findIntersect' :: @ radial_origin * theta * [line_segment] -> Maybe Point @
--
-- Find the first intersection of a line through @radial_origin@ 
-- at angle @theta@ and the supplied line segments, if there 
-- is one. 
--
findIntersect :: (Floating u, Real u, Ord u, FromPtSize u)
               => Point2 u -> Radian -> [LineSegment u] 
               -> Maybe (Point2 u)
findIntersect radial_ogin ang = step 
  where
    plane       = makePlane radial_ogin ang
    step []     = Nothing
    step (x:xs) = case interLinesegLine x plane of 
                     Just pt | quadrantCheck ang radial_ogin pt -> Just pt
                     _       -> step xs


-- | The tolerance on Radian equality should be acceptable...
--
quadrantCheck :: (Real u, Floating u) 
              => Radian -> Point2 u -> Point2 u -> Bool
quadrantCheck theta ctr pt = theta == lineAngle ctr pt





-- | 'makePlane' : @ point * ang -> Line @
--
-- Make an infinite line \/ plane passing through the supplied 
-- with elevation @ang@.
--
makePlane :: Floating u => Point2 u -> Radian -> (Point2 u, Point2 u)
makePlane radial_ogin ang = (radial_ogin, radial_ogin .+^ avec ang 100)

-- | 'rectangleLineSegments' : @ half_width * half_height -> [LineSegment] @
--
-- Compute the line segments of a rectangle.
--
rectangleLineSegments :: Num u => u -> u -> Point2 u -> [LineSegment u]
rectangleLineSegments hw hh ctr = [(br,tr), (tr,tl), (tl,bl), (bl,br)]
  where
    br = ctr .+^ vec hw    (-hh)
    tr = ctr .+^ vec hw    hh
    tl = ctr .+^ vec (-hw) hh
    bl = ctr .+^ vec (-hw) (-hh)


-- | 'polygonleLineSegments' : @ [point] -> [LineSegment] @
--
-- Compute the line segments of a polygon fome a list of 
-- its vertices.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list. 
-- 
polygonLineSegments :: [Point2 u] -> [LineSegment u]
polygonLineSegments []     = error "polygonLineSegments - emptyList"
polygonLineSegments (x:xs) = step x xs 
  where
    step a []        = [(a,x)]
    step a (b:bs)    = (a,b) : step b bs

