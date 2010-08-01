{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Utils.Intersection
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Intersection of line to line and line to plane
-- 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Utils.Intersection
  ( 
    LineSegment(..)
  , PointSlope
  , pointSlope
  , LineEqn
  , lineEqn
  , toLineEqn
  , findIntersect
  , intersection

  , firstIntersect
  , lineIntersect
  , planeIntersect
  , radialSegment
  ) 
  where

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

data LineSegment u = LS (Point2 u) (Point2 u)
  deriving (Eq,Ord,Show)


data PointSlope u = PointSlope 
      { _point_slope_point :: Point2 u
      , _point_slope_slope :: u
      }
  deriving (Eq,Show)

pointSlope :: Fractional u => Point2 u -> Radian -> PointSlope u 
pointSlope pt theta = PointSlope pt (fromRadian $ tan theta)


-- | Line in equational form, i.e. @Ax + By + C = 0@.
data LineEqn u = LineEqn 
      { _line_eqn_A :: !u
      , _line_eqn_B :: !u
      , _line_eqn_C :: !u 
      }
  deriving (Eq,Show)

lineEqn :: Num u => Point2 u -> Point2 u -> LineEqn u
lineEqn (P2 x1 y1) (P2 x2 y2) = LineEqn a b c 
  where
    a = y1 - y2
    b = x2 - x1
    c = (x1*y2) - (x2*y1)


toLineEqn :: Num u => PointSlope u -> LineEqn u
toLineEqn (PointSlope (P2 x0 y0) m) = LineEqn m (-1) ((-m) * x0 + y0)




data IntersectionResult u = Intersects u u | Contained | NoIntersect
  deriving (Eq,Show)


-- Note the uses a /plane/ so is susceptible to picking the 
-- wrong quadrant...
--
findIntersect :: (Floating u, Ord u)
               => Point2 u -> Radian -> [LineSegment u] -> Maybe (Point2 u)
findIntersect ctr theta = step 
  where
    eqn         = toLineEqn $ pointSlope ctr theta
    step []     = Nothing
    step (x:xs) = maybe (step xs) Just $ intersection x eqn
   


intersection :: (Fractional u, Ord u) 
             => LineSegment u -> LineEqn u -> Maybe (Point2 u)
intersection ls@(LS p q) eqn = case intersect1 ls eqn of
    Intersects fp fq -> let t = fp / (fp-fq) in Just $ affineComb p q t 
    Contained        -> Just p
    NoIntersect      -> Nothing



intersect1 :: (Num u, Ord u) 
           => LineSegment u -> LineEqn u -> IntersectionResult u
intersect1 (LS p q) eqn = 
     if inters fp fq then Intersects fp fq
        else if contained fp fq then Contained else NoIntersect
  where
    inters a b    = (a < 0 && b >= 0) || (a > 0 && b <= 0)
    contained a b = a == 0 && b == 0
    fp            = lineF p eqn
    fq            = lineF q eqn
 
lineF :: Num u => Point2 u -> LineEqn u -> u
lineF (P2 x y) (LineEqn a b c) = a*x + b*y + c

affineComb :: Num u => Point2 u -> Point2 u -> u -> Point2 u
affineComb p q t = p .+^ t *^ (q .-. p)

--------------------------------------------------------------------------------
-- OLD...

firstIntersect :: (Floating u, Ord u)
               => Point2 u -> Radian -> [LineSegment u] -> Maybe (Point2 u)
firstIntersect ctr theta = step where
   step []     = Nothing
   step (x:xs) = maybe (step xs) Just $ planeIntersect ctr theta x


-- quadrant 0 - 0..90, 1 - 90..180, 2 - 180..270, 3 - 270..360

quadrant :: Radian -> Int
quadrant = step . circularModulo
  where
    step x | x < pi/2   = 1
           | x < pi     = 2
           | x < 3*pi/2 = 3
           | otherwise  = 4


-- this isn't good enough...
radialSegment :: (Floating u, Ord u) 
              => Radian -> Point2 u -> u -> u -> LineSegment u
radialSegment theta ctr x0 x1 = LS ctr (ctr .+^ avec theta len) 
  where
    len = case quadrant theta of
            1 -> (max x0 x1) / fromRadian (cos theta)
            2 -> negate $ (min x0 x1) / fromRadian (cos $ pi - theta)
            3 -> negate $ (min x0 x1) / fromRadian (cos $ theta - pi)
            _ -> (max x0 x1) / fromRadian (cos $ 2*pi - theta) 


planeIntersect :: (Floating u, Ord u) 
               => Point2 u -> Radian -> LineSegment u -> Maybe (Point2 u)
planeIntersect ctr theta l2@(LS (P2 x0 _) (P2 x1 _)) = 
    lineIntersect (radialSegment theta ctr x0 x1) l2

lineIntersect :: (Fractional u, Ord u) 
              => LineSegment u -> LineSegment u -> Maybe (Point2 u)
lineIntersect (LS (P2 ax0 ay0) (P2 ax1 ay1)) (LS (P2 bx0 by0) (P2 bx1 by1)) = 
    if denom/=0 && (0 <= ua && ua <= 1) && (0 <= ub && ub <= 1) 
       then Just (P2 x y) else Nothing
  where
    denom  = (by1 - by0) * (ax1 - ax0) - (bx1 - bx0) * (ay1 - ay0)

    numera = (bx1 - bx0) * (ay0 - by0) - (by1 - by0) * (ax0 - bx0)
    numerb = (ax1 - ax0) * (ay0 - by0) - (ay1 - by0) * (ax0 - bx0)

    ua     = numera / denom
    ub     = numerb / denom
    x      = ax0 + ua * (ax1 - ax0)
    y      = ay0 + ua * (ay1 - ay0)
