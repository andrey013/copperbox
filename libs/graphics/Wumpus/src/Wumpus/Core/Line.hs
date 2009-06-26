{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
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


module Wumpus.Core.Line 
  (
  -- * Line types
    LineSegment(..)
  , DLineSegment2

  -- * Construction
  , lineTo
  , hline
  , vline

  -- * Operations
  , opposite
  , gradient
  , segmentLength

  ) where

import Wumpus.Core.Instances
import Wumpus.Core.Matrix
import Wumpus.Core.Point

import Data.AffineSpace
import Data.VectorSpace



data LineSegment (pt :: * -> *) a = LS (pt a) (pt a)
  deriving (Eq,Show)

type DLineSegment2 = LineSegment Point2 Double


instance Functor pt => Functor (LineSegment pt) where
  fmap f (LS p p') = LS (fmap f p) (fmap f p')

 
instance VecMult Matrix3'3 (LineSegment Point2) where
  (*#) m3'3 (LS p p') = LS (m3'3 *# p) (m3'3 *# p')

instance Pointwise (LineSegment Point2 a) where
  type Pt (LineSegment Point2 a) = Point2 a
  pointwise f (LS p p') = LS (f p) (f p')


--------------------------------------------------------------------------------

-- construction
lineTo :: pt a -> pt a -> LineSegment pt a
lineTo = LS --  p1 v where v = p2 .-. p1


hline :: Num a => Point2 a -> a -> LineSegment Point2 a
hline p@(P2 x y) a = LS p (P2 (x+a) y)

vline :: Num a => Point2 a -> a -> LineSegment Point2 a
vline p@(P2 x y) a = LS p (P2 x (y+a))

--------------------------------------------------------------------------------
-- operations

opposite :: LineSegment pt a -> LineSegment pt a
opposite (LS p p') = LS p' p

gradient :: Floating a => LineSegment Point2 a -> a
gradient (LS (P2 x y) (P2 x' y')) = (y'-y) / (x'-x) 



segmentLength :: (Floating (Scalar (Diff (pt a))), 
                  InnerSpace (Diff (pt a)), AffineSpace (pt a) )
              => LineSegment pt a -> Scalar (Diff (pt a))

segmentLength (LS p p') = distance p' p

