{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Intersection
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Intersection of Paths with (infinite) lines.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Intersection
  ( 

    Line(..)
  , inclinedLine
  , vectorLine

  , lineLineIntersection
  , linePathIntersection
  , linePathSegmentIntersection

  ) where

import Wumpus.Drawing.Paths.Base


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

--------------------------------------------------------------------------------


--
-- Private types - LineEquation, 2x2 matrix and bezier curve.
--
-- Although these types are /general/ exposing them just leads 
-- to a bloated API.
--
-- If these types are fund to be more useful - they can go into 
-- @Drawing.Basis@.
--


-- | Line in equational form, i.e. @Ax + By + C = 0@.
--
data LineEquation u = LineEquation 
      { _line_eqn_A :: !u
      , _line_eqn_B :: !u
      , _line_eqn_C :: !u 
      }
  deriving (Eq,Show)

type instance DUnit (LineEquation u) = u


-- | 'lineEquation' : @ point1 * point2 -> LineEquation @
-- 
-- Construct a line in equational form bisecting the supplied 
-- points.
--
lineEquation :: Num u => Point2 u -> Point2 u -> LineEquation u
lineEquation (P2 x1 y1) (P2 x2 y2) = LineEquation a b c 
  where
    a = y1 - y2
    b = x2 - x1
    c = (x1*y2) - (x2*y1)

-- | 2x2 matrix, considered to be in row-major form.
-- 
-- > (M2'2 a b
-- >       c d)
--
-- 

data Matrix2'2 u = M2'2 !u !u   !u !u
  deriving (Eq)

type instance DUnit (Matrix2'2 u) = u


-- | Determinant of a 2x2 matrix.
--
det2'2 :: Num u => Matrix2'2 u -> u
det2'2 (M2'2 a b c d) = a*d - b*c



-- | A Strict cubic Bezier curve.
--
data BezierCurve u = BezierCurve !(Point2 u) !(Point2 u) !(Point2 u) !(Point2 u)
  deriving (Eq,Ord,Show)

type instance DUnit (BezierCurve u) = u


--------------------------------------------------------------------------------

-- | Infinite line represented by two points.
--
data Line u = Line (Point2 u) (Point2 u)
  deriving (Eq,Show)

type instance DUnit (Line u) = u



-- | 'inclinedLine' : @ point * ang -> Line @
--


-- Make an infinite line passing through the supplied point 
-- inclined by @ang@.

--
inclinedLine :: Floating u => Point2 u -> Radian -> Line u
inclinedLine radial_ogin ang = Line radial_ogin (radial_ogin .+^ avec ang 100)

vectorLine :: Num u => Vec2 u -> Point2 u -> Line u
vectorLine v1 p0 = Line p0 (p0 .+^ v1)




-- | 'interLineLine' : @ line1 * line2 -> Maybe Point @
-- 
-- Find the intersection of two lines, if there is one. 
--
-- Lines are infinite they are represented by points on them, 
-- they are not line segments.
--
-- An answer of @Nothing@ may indicate either the lines coincide
-- or the are parallel.
--
lineLineIntersection :: Fractional u 
                 => Line u -> Line u -> Maybe (Point2 u)
lineLineIntersection (Line p1 p2) (Line q1 q2) = 
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


linePathIntersection :: (Floating u, Ord u, Tolerance u) 
                     => Line u -> AbsPath u -> Maybe (Point2 u)
linePathIntersection ln = step . pathViewL
  where
    step EmptyPathL = Nothing
    step (a :<< bs) = let ans = linePathSegmentIntersection ln a
                      in case ans of
                         Nothing -> step (pathViewL bs)
                         _       -> ans

linePathSegmentIntersection :: (Floating u, Ord u, Tolerance u) 
                            => Line u -> PathSegment u -> Maybe (Point2 u)
linePathSegmentIntersection ln1 (LineSeg _ p0 p1)        = 
    lineLineIntersection ln1 (Line p0 p1)

linePathSegmentIntersection (Line pa pb) (CurveSeg _ p0 p1 p2 p3) = 
    lineEqnCurveIntersection (lineEquation pa pb) (BezierCurve p0 p1 p2 p3)

lineEqnCurveIntersection :: (Floating u, Ord u, Tolerance u) 
                         => LineEquation u -> BezierCurve u -> Maybe (Point2 u)
lineEqnCurveIntersection eqnline c0 = step c0
  where
    step c  = case cut eqnline c of
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
cut :: (Floating u , Ord u, Tolerance u)
    => LineEquation u -> BezierCurve u -> Either (Point2 u) Bool
cut eqnline (BezierCurve p0 p1 p2 p3) = 
    if d0 `tEQ` 0 then Left p0 else
    if d3 `tEQ` 0 then Left p3 else
    let ds = [d0,d1,d2,d3] in Right $ not $ all pve ds || all nve ds
  where
    pve = (>= 0)
    nve = (< 0)
    d0  = pointLineDistance p0 eqnline 
    d1  = pointLineDistance p1 eqnline 
    d2  = pointLineDistance p2 eqnline 
    d3  = pointLineDistance p3 eqnline 



-- | Curve subdivision via de Casteljau\'s algorithm.
--
subdivide :: Fractional u 
          => BezierCurve u -> (BezierCurve u, BezierCurve u)
subdivide (BezierCurve p0 p1 p2 p3) =
    (BezierCurve p0 p01 p012 p0123, BezierCurve p0123 p123 p23 p3)
  where
    p01   = midpoint p0    p1
    p12   = midpoint p1    p2
    p23   = midpoint p2    p3
    p012  = midpoint p01   p12
    p123  = midpoint p12   p23
    p0123 = midpoint p012  p123

-- | 'midpoint' : @ start_point * end_point -> Midpoint @
-- 
-- Mid-point on the line formed between the two supplied points.
--
midpoint :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


-- | 'pointLineDistance' : @ point -> line -> Distance @
--
-- Find the distance from a point to a line in equational form
-- using this formula:
-- 
-- > P(u,v) 
-- > L: Ax + By + C = 0
-- >
-- > (A*u) + (B*v) + C 
-- > -----------------
-- > sqrt $ (A^2) +(B^2)
--
-- A positive distance indicates the point is above the line, 
-- negative indicates below.
--
pointLineDistance :: Floating u => Point2 u -> LineEquation u -> u
pointLineDistance (P2 u v) (LineEquation a b c) = 
    ((a*u) + (b*v) + c) / base
  where
    base = sqrt $ (a^two) + (b^two)
    two  :: Integer
    two  = 2