{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Line
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Line segment
--
--------------------------------------------------------------------------------


module Wumpus.Core.Line where

import Wumpus.Core.Instances
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace


-- To determine...
-- (pt x vec) or (pt x pt)?


data LineSegment a = LS (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DLineSegment = LineSegment Double


instance Functor LineSegment where
  fmap f (LS pt vec) = LS (fmap f pt) (fmap f vec)

 
instance VecMult Matrix3'3 LineSegment where
  (*#) m3'3 (LS p p') = LS (m3'3 *# p) (m3'3 *# p')


-- construct
lineTo :: Point2 a -> Point2 a -> LineSegment a
lineTo = LS --  p1 v where v = p2 .-. p1


hline :: Num a => Point2 a -> a -> LineSegment a
hline p@(P2 x y) a = LS p (P2 (x+a) y)

vline :: Num a => Point2 a -> a -> LineSegment a
vline p@(P2 x y) a = LS p (P2 x (y+a))


-- operations

{-
midpoint :: (Fractional a, AffineSpace a, Diff a ~ a) => LineSegment a -> Point2 a
midpoint (LS p p') = p .+^ ((p' .-. p)/2) 
-}

segmentLength :: (Floating a, InnerSpace a,  AffineSpace a, 
                  Diff a ~ a, a ~ Scalar a )  
              => LineSegment a -> a
segmentLength (LS p p') = distance p' p
