{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Geometry.Intersection
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Intersection of line to line and line to plane
-- 
-- \*\* - WARNING \*\* - half baked. 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Geometry.Intersection
  ( 

    lineIntersection
  , linesegIntersection

  -- * OLD...
  , LineSegment(..)
  , PointSlope
  , pointSlope
  , toLineEquation
  , findIntersect
  , intersection

  , rectangleLines
  , polygonLines

  ) 
  where

import Wumpus.Drawing.Geometry.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


-- | 'lineIntersection' : @ line1 * line2 -> Maybe Point @
-- 
-- Find the intersection of two lines, if there is one. 
--
-- Lines are infinite they are represented by points on them, 
-- they are not line segments.
--
-- An answer of @Nothing@ may indicate wither the lines coincide
-- or the are parallel.
--
lineIntersection :: Fractional u 
                 => (Point2 u, Point2 u) -> (Point2 u, Point2 u) 
                 -> Maybe (Point2 u)
lineIntersection (p1,p2) (q1,q2) = 
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



-- | 'lineIntersection' : @ line1 * line2 -> Maybe Point @
-- 
-- Find the intersection of two lines, if there is one. 
--
-- Lines are infinite they are represented by points on them, 
-- they are not line segments.
--
-- An answer of @Nothing@ may indicate wither the lines coincide
-- or the are parallel.
--
linesegIntersection :: (Fractional u, Ord u)
                    => (Point2 u, Point2 u) -> (Point2 u, Point2 u) 
                    -> Maybe (Point2 u)
linesegIntersection l1 l2 = lineIntersection l1 l2 >>= segcheck
  where
    segcheck pt = if within pt l1 && within pt l2 then Just pt else Nothing
   
    within (P2 x y) ((P2 x0 y0), (P2 x1 y1)) =  between x (ordpair x0 x1)
                                             && between y (ordpair y0 y1)

    ordpair a b     = (min a b, max a b)
    between a (s,t) = s <= a && a <= t  






-- WARNING - This module is not very good (neither particularly 
-- robust, nor efficient).
-- 
-- I really need to find an algorithm that does this properly.
--

data LineSegment u = LS (Point2 u) (Point2 u)
  deriving (Eq,Ord,Show)


data PointSlope u = PointSlope 
      { _point_slope_point :: Point2 u
      , _point_slope_slope :: u
      }
  deriving (Eq,Show)

pointSlope :: Fractional u => Point2 u -> Radian -> PointSlope u 
pointSlope pt theta = PointSlope pt (fromRadian $ tan theta)



toLineEquation :: Num u => PointSlope u -> LineEquation u
toLineEquation (PointSlope (P2 x0 y0) m) = LineEquation m (-1) ((-m) * x0 + y0)




data IntersectionResult u = Intersects u u | Contained | NoIntersect
  deriving (Eq,Show)


-- Note the uses a /plane/ so is susceptible to picking the 
-- wrong quadrant...
--
findIntersect :: (Floating u, Real u, Ord u)
               => Point2 u -> Radian -> [LineSegment u] -> Maybe (Point2 u)
findIntersect ctr ang0 = step 
  where
    theta       = circularModulo ang0
    eqn         = toLineEquation $ pointSlope ctr theta
    step []     = Nothing
    step (x:xs) = case intersection x eqn of 
                     Just pt | quadrantCheck theta ctr pt -> Just pt
                     _       -> step xs


quadrantCheck :: (Real u, Floating u) 
              => Radian -> Point2 u -> Point2 u -> Bool
quadrantCheck theta ctr pt = theta == lineAngle ctr pt

intersection :: (Real u, Fractional u, Ord u) 
             => LineSegment u -> LineEquation u -> Maybe (Point2 u)
intersection ls@(LS p q) eqn = case intersect1 ls eqn of
    Intersects fp fq -> let t = fp / (fp-fq) in Just $ affineComb t p q
    Contained        -> Just p
    NoIntersect      -> Nothing



intersect1 :: (Num u, Ord u) 
           => LineSegment u -> LineEquation u -> IntersectionResult u
intersect1 (LS p q) eqn = 
     if inters fp fq then Intersects fp fq
                     else if contained fp fq then Contained else NoIntersect
  where
    inters a b    = (a < 0 && b >= 0) || (a > 0 && b <= 0)
    contained a b = a == 0 && b == 0
    fp            = lineF p eqn
    fq            = lineF q eqn
 
lineF :: Num u => Point2 u -> LineEquation u -> u
lineF (P2 x y) (LineEquation a b c) = a*x + b*y + c

-- affineComb :: Num u => Point2 u -> Point2 u -> u -> Point2 u
-- affineComb p q t = p .+^ t *^ (q .-. p)




rectangleLines :: Num u => Point2 u -> u -> u -> [LineSegment u]
rectangleLines ctr hw hh = [LS br tr, LS tr tl, LS tl bl, LS bl br]
  where
    br = ctr .+^ (vec hw    (-hh))
    tr = ctr .+^ (vec hw    hh)
    tl = ctr .+^ (vec (-hw) hh)
    bl = ctr .+^ (vec (-hw) (-hh))


polygonLines :: [Point2 u] -> [LineSegment u]
polygonLines []     = error "polygonLines - emptyList"
polygonLines (x:xs) = step x xs 
  where
    step a []        = [LS a x]
    step a (b:bs)    = LS a b : step b bs



