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

-- To determine...
-- (pt x vec) or (pt x pt)?


data LineSegment a = LS (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DLineSegment = LineSegment Double


instance Functor LineSegment where
  fmap f (LS pt vec) = LS (fmap f pt) (fmap f vec)

lineTo :: Point2 a -> Point2 a -> LineSegment a
lineTo = LS --  p1 v where v = p2 .-. p1


hline :: Num a => Point2 a -> a -> LineSegment a
hline p@(P2 x y) a = LS p (P2 (x+a) y)

vline :: Num a => Point2 a -> a -> LineSegment a
vline p@(P2 x y) a = LS p (P2 x (y+a))



-- To sort out - point has a @midpoint@ function
-- Vocabulary wise is 'midpoint' the midpoint of a line-segment 
-- or the midpoint between 2 points?  

midpointL :: (Fractional a, AffineSpace a, Diff a ~ a) => LineSegment a -> Point2 a
midpointL (LS p p') = p .+^ ((p' .-. p)/2) 


-- Not really what I want...
-- a translation shouldn't change the vectorial but of a line segment
 
instance VecMult Matrix3'3 LineSegment where
  (*#) m3'3 (LS p p') = LS (m3'3 *# p) (m3'3 *# p')
