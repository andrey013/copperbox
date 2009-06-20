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


data LineSegment2 a = LS2 (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DLineSegment2 = LineSegment2 Double


instance Functor LineSegment2 where
  fmap f (LS2 p p') = LS2 (fmap f p) (fmap f p')

 
instance VecMult Matrix3'3 LineSegment2 where
  (*#) m3'3 (LS2 p p') = LS2 (m3'3 *# p) (m3'3 *# p')


-- construct
lineTo :: Point2 a -> Point2 a -> LineSegment2 a
lineTo = LS2 --  p1 v where v = p2 .-. p1


hline :: Num a => Point2 a -> a -> LineSegment2 a
hline p@(P2 x y) a = LS2 p (P2 (x+a) y)

vline :: Num a => Point2 a -> a -> LineSegment2 a
vline p@(P2 x y) a = LS2 p (P2 x (y+a))


-- operations

opposite :: LineSegment2 a -> LineSegment2 a
opposite (LS2 p p') = LS2 p' p

gradient :: Floating a => LineSegment2 a -> a
gradient (LS2 (P2 x y) (P2 x' y')) = (y'-y) / (x'-x) 


segmentLength :: (Floating a, InnerSpace a,  AffineSpace a, 
                  Diff a ~ a, a ~ Scalar a )  
              => LineSegment2 a -> a
segmentLength (LS2 p p') = distance p' p
