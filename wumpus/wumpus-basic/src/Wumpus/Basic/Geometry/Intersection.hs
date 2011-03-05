{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Geometry.Intersection
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

module Wumpus.Basic.Geometry.Intersection
  ( 

    LineSegment
  , interLineLine
  , interLinesegLineseg
  , interLinesegLine
  , interCurveLine

  , findIntersect
  , makePlane
  , rectangleLineSegments
  , polygonLineSegments


  ) 
  where

import Wumpus.Basic.Geometry.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space



-- Potentially lines hould be a new datatype.


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
interLinesegLineseg :: (Fractional u, Ord u, PsDouble u)
                    => LineSegment u -> LineSegment u -> Maybe (Point2 u)
interLinesegLineseg a@(LineSegment p q) b@(LineSegment s t) = 
    interLineLine (p,q) (s,t) >>= segcheck
  where
    segcheck = mbCheck (\pt -> withinPoints pt a && withinPoints pt b)
 

-- | 'interLinesegLine' : @ line_segment * line -> Maybe Point @
-- 
-- Find the intersection of a line and a line segment, if there 
-- is one. 
--
-- An answer of @Nothing@ indicates that the the line and line
-- segment coincide, or that there is no intersection.
--
interLinesegLine :: (Fractional u, Ord u, PsDouble u)
                 => LineSegment u -> (Point2 u, Point2 u) -> Maybe (Point2 u)
interLinesegLine a@(LineSegment p q) line = 
    interLineLine (p,q) line >>= segcheck
  where
    segcheck = mbCheck (\pt -> withinPoints pt a)


mbCheck :: (a -> Bool) -> a -> Maybe a
mbCheck test a = if test a then Just a else Nothing

-- | Check the point is \"within\" the span of the line.
--
-- Note - this function is to be used \*after\* an intersection
-- has been found. Hence it is not export.
--
withinPoints :: (Ord u, PsDouble u) => Point2 u -> LineSegment u -> Bool
withinPoints (P2 x y) (LineSegment (P2 x0 y0) (P2 x1 y1)) =  
    between x (ordpair x0 x1) && between y (ordpair y0 y1)
  where
    ordpair a b     = (min a b, max a b)
    between a (s,t) = (s `tGT` a) && (a `tGT` t)

    tGT a b         = a < b || abs (a-b) < tolerance

-- | Note - its important to use tolerance for the @withPoints@ 
-- function.
--
tolerance :: PsDouble u => u
tolerance = fromPsDouble 0.01






--------------------------------------------------------------------------------
-- intersection of line and Bezier curve

interCurveLine :: (Floating u , Ord u, PsDouble u)
               => BezierCurve u -> (Point2 u, Point2 u) -> Maybe (Point2 u)
interCurveLine c0 (p,q) = step c0
  where
    eqline  = lineEquation p q
    step c  = case cut c eqline of
                Left pt     -> Just pt      -- cut at start or end
                Right False -> Nothing
                Right True  -> let (a,b) = subdivide c
                               in case step a of
                                   Just pt -> Just pt
                                   Nothing -> step b
 
-- | Is the curve cut by the line? 
--
-- The curve might cut at the start or end points - which is good
-- as it saves performing a subdivision. But make the return type
-- a bit involved.
--
cut :: (Floating u , Ord u, PsDouble u)
    => BezierCurve u -> LineEquation u -> Either (Point2 u) Bool
cut (BezierCurve p0 p1 p2 p3) line = 
    if d0 `tEQ` 0 then Left p0 else
    if d3 `tEQ` 0 then Left p3 else
    let ds = [d0,d1,d2,d3] in Right $ not $ all pve ds || all nve ds
  where
    tEQ = \a b -> abs (a-b) < tolerance
    pve = \a -> a > tolerance
    nve = \a -> a < (negate tolerance)
    d0  = pointLineDistance p0 line 
    d1  = pointLineDistance p1 line 
    d2  = pointLineDistance p2 line 
    d3  = pointLineDistance p3 line 



    


--------------------------------------------------------------------------------
-- Intersection of shape boundaries...



-- | 'findIntersect' :: @ radial_origin * theta * [line_segment] -> Maybe Point @
--
-- Find the first intersection of a line through @radial_origin@ 
-- at angle @theta@ and the supplied line segments, if there 
-- is one. 
--
findIntersect :: (Floating u, Real u, Ord u, PsDouble u)
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


