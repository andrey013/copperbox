{-# LANGUAGE TypeFamilies               #-}
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

import Wumpus.Core.Fun
import Wumpus.Core.Line
import Wumpus.Core.Point

import Data.AffineSpace
import Data.VectorSpace

data Curve a = Curve (Point2 a) (Point2 a) (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DCurve = Curve Double


instance Functor Curve where
  fmap f (Curve p1 cp1 cp2 p2) = 
    Curve (fmap f p1) (fmap f cp1) (fmap f cp2) (fmap f p2)




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