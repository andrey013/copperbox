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
  , firstIntersect
  , lineIntersect
  , planeIntersect
  , radialSegment
  ) 
  where

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

data LineSegment u = LS (Point2 u) (Point2 u)
  deriving (Eq,Ord,Show)


firstIntersect :: (Floating u, Ord u)
               => Point2 u -> Radian -> [LineSegment u] -> Maybe (Point2 u)
firstIntersect ctr theta = step where
   step []     = Nothing
   step (x:xs) = maybe (step xs) Just $ planeIntersect ctr theta x


-- quadrant 0 - 0..90, 1 - 90..180, 2 - 180..270, 3 - 270..360

quadrant :: Radian -> Int
quadrant = step . circularModulo
  where
    step x | x < pi/2   = 0
           | x < pi     = 1
           | x < 3*pi/2 = 2
           | otherwise  = 3


-- this isn't good enough...
radialSegment :: (Floating u, Ord u) 
              => Radian -> Point2 u -> u -> u -> LineSegment u
radialSegment theta ctr x0 x1 = LS ctr (ctr .+^ avec theta len) 
  where
    len = case quadrant theta of
            0 -> (max x0 x1) / fromRadian (cos theta)
            1 -> negate $ (min x0 x1) / fromRadian (cos $ pi - theta)
            2 -> negate $ (min x0 x1) / fromRadian (cos $ theta - pi)
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
