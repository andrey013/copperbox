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

  , rectangleLines

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
findIntersect :: (Floating u, Real u, Ord u)
               => Point2 u -> Radian -> [LineSegment u] -> Maybe (Point2 u)
findIntersect ctr theta = step 
  where
    eqn         = toLineEqn $ pointSlope ctr theta
    step []     = Nothing
    step (x:xs) = case intersection x eqn of 
                     Just pt | quadrantCheck theta ctr pt -> Just pt
                     _       -> step xs


quadrantCheck :: (Real u, Floating u) 
              => Radian -> Point2 u -> Point2 u -> Bool
quadrantCheck theta ctr pt = theta `req` langle ctr pt

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





rectangleLines :: Num u => Point2 u -> u -> u -> [LineSegment u]
rectangleLines ctr hw hh = [LS br tr, LS tr tl, LS tl bl, LS bl br]
  where
    br = ctr .+^ (vec hw    (-hh))
    tr = ctr .+^ (vec hw    hh)
    tl = ctr .+^ (vec (-hw) hh)
    bl = ctr .+^ (vec (-hw) (-hh))
