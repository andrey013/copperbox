{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  , line
  , lineTo
  , hline
  , vline
  , aline

  -- * Operations
  , opposite
  , langle
  , segmentLength
  , lineCenter

  ) where

import Wumpus.Core.Geometric
import Wumpus.Core.Instances ()
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

import Control.Applicative ( pure )


--------------------------------------------------------------------------------
-- Line types and standard instances


data LineSegment (pt :: * -> *) a = LS (pt a) (pt a)
  deriving (Eq,Show)

type DLineSegment2 = LineSegment Point2 Double


instance Functor pt => Functor (LineSegment pt) where
  fmap f (LS p p') = LS (fmap f p) (fmap f p')

 
instance MatrixMult Matrix3'3 (LineSegment Point2) where
  (*#) m3'3 (LS p p') = LS (m3'3 *# p) (m3'3 *# p')

instance Pointwise (LineSegment Point2 a) where
  type Pt (LineSegment Point2 a) = Point2 a
  pointwise f (LS p p') = LS (f p) (f p')

--------------------------------------------------------------------------------
-- Other instances


-- This drags in Undecidable Instances...

instance (Floating (Scalar (Diff (pt a))), 
          InnerSpace (Diff (pt a)), AffineSpace (pt a) )
    => Congruent (LineSegment pt a) where
  congruent l l' = segmentLength l == segmentLength l' 



--------------------------------------------------------------------------------

-- construction

line :: AffineSpace (pt a) => pt a -> Diff (pt a) -> LineSegment pt a
line p v = LS p (p .+^ v)

lineTo :: pt a -> pt a -> LineSegment pt a
lineTo = LS --  p1 v where v = p2 .-. p1

-- | Horizontal line from point @p@ of length @a@ .
hline :: Num a => Point2 a -> a -> LineSegment Point2 a
hline p@(P2 x y) a = LS p (P2 (x+a) y)

-- | Vertical line from point @p@ of length @a@.
vline :: Num a => Point2 a -> a -> LineSegment Point2 a
vline p@(P2 x y) a = LS p (P2 x (y+a))


-- | A line from point @p@ in the direction @theta@ from x-axis
-- of length @a@
aline :: (Floating a, AffineSpace (pt a), Vec2 a ~ Diff (pt a)) 
      => pt a -> Radian a -> a -> LineSegment pt a
aline p theta a = LS p (p .+^ vec2 theta a)

--------------------------------------------------------------------------------
-- operations







-- | Reverse the direction of a line
opposite :: LineSegment pt a -> LineSegment pt a
opposite (LS p p') = LS p' p

-- | Angle ccw from x-axis
langle :: Floating a => LineSegment Point2 a -> Radian a
langle (LS (P2 x y) (P2 x' y')) = pure $ atan $ (y'-y) / (x'-x) 



segmentLength :: (Floating (Scalar (Diff (pt a))), 
                  InnerSpace (Diff (pt a)), AffineSpace (pt a) )
              => LineSegment pt a -> Scalar (Diff (pt a))

segmentLength (LS p p') = distance p' p

lineCenter :: (Fractional (Scalar (Diff (pt a))), 
               AffineSpace (pt a), VectorSpace (Diff (pt a)))
           => LineSegment pt a -> pt a
lineCenter (LS p p') = midpoint p' p


