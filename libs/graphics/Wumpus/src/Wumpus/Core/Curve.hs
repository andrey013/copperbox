{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Curve
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Cubic bezier curves
--
--------------------------------------------------------------------------------


module Wumpus.Core.Curve where

import Wumpus.Core.Instances ()
import Wumpus.Core.Point
import Wumpus.Core.VSExtra

import Data.AffineSpace
import Data.VectorSpace

data Curve a = Curve (Point2 a) (Point2 a) (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DCurve = Curve Double


instance Functor Curve where
  fmap f (Curve p0 p1 p2 p3) = 
    Curve (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)




-- de casteljau's algorithm
subdivide :: (Fractional (Scalar (Diff a)), Num (Diff a),
              VectorSpace (Diff a),  AffineSpace a)  
          => Curve a -> (Curve a, Curve a)
subdivide (Curve p0 p1 p2 p3) = 
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = midpoint p0    p1
    p12   = midpoint p1    p2
    p23   = midpoint p2    p3
    p012  = midpoint p01   p12
    p123  = midpoint p12   p23
    p0123 = midpoint p012  p123



{-

----

-- Shemanarev's algorithm




simpleSmooth xs = ys where
  ys = innerLines $ midpoints xs
  ps = outerProportions xs


midpoints :: (AffineSpace a, Fractional a, Diff a ~ a) 
          => [LineSegment a] -> [Point2 a]
midpoints = map midpoint 

innerLines :: [Point2 a] -> [LineSegment a]
innerLines = twomap lineTo

outerProportions :: (Floating a, InnerSpace a,  AffineSpace a, 
                     Diff a ~ a, a ~ Scalar a )  
                 => [LineSegment a] -> [a]
outerProportions = twomap fn where
  fn l1 l2 = segmentLength l1 / segmentLength l2


-- divideInner :: LineSegment a -> a -> (Point2 a, Point2 a, Point2 a)
-- divideInner 
-}