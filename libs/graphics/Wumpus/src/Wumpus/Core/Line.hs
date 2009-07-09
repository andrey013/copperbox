{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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
-- Line segments and polylines
--
--------------------------------------------------------------------------------


module Wumpus.Core.Line 
  (
  -- * Line types
  -- ** Line segments
    LineSegment(..)
  , DLineSegment2
  , CoLineSegment
  , DCoLineSegment2

  -- ** Poly lines
  , PolyLine(..)
  , DPolyLine2
  , CoPolyLine
  , DCoPolyLine2

   -- * Construction
  , line
  , hline
  , vline
  , aline

  , lineTo

  -- * Operations
  , converse
  , langle
  , segmentLength
  , lineCenter
  , expandLine
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


--------------------------------------------------------------------------------
-- Line types and standard instances


data LineSegment (pt :: * -> *) a = LS (pt a) (pt a)
  deriving (Eq,Show)

-- LineSegment in 2-space.
type DLineSegment2 = LineSegment Point2 Double


type CoLineSegment pt a = pt a -> LineSegment pt a  

type DCoLineSegment2 = CoLineSegment Point2 Double

-- Poly Lines

data PolyLine (pt :: * -> *) a = PolyLine [pt a]
  deriving (Eq,Show)

-- | PolyLine in 2-space.
type DPolyLine2 = PolyLine Point2 Double


type CoPolyLine pt a = pt a -> PolyLine pt a  

type DCoPolyLine2 = CoPolyLine Point2 Double


instance Functor pt => Functor (LineSegment pt) where
  fmap f (LS p p') = LS (fmap f p) (fmap f p')

 
instance MatrixMult Matrix3'3 (LineSegment Point2) where
  (*#) m3'3 (LS p p') = LS (m3'3 *# p) (m3'3 *# p')



instance Functor pt => Functor (PolyLine pt) where
  fmap f (PolyLine ps) = PolyLine (map (fmap f) ps)


instance MatrixMult Matrix3'3 (PolyLine Point2) where
  (*#) m3'3 (PolyLine ps) = PolyLine (map (m3'3 *#) ps)


instance Pointwise (LineSegment Point2 a) where
  type Pt (LineSegment Point2 a) = Point2 a
  pointwise f (LS p p') = LS (f p) (f p')

instance Pointwise (CoLineSegment pt a) where
  type Pt (CoLineSegment pt a) = pt a
  pointwise f pf = pf . f

instance Pointwise (PolyLine Point2 a) where
  type Pt (PolyLine Point2 a) = Point2 a
  pointwise f (PolyLine ps) = PolyLine (map f ps)


instance ExtractPoints (LineSegment Point2 a) where
  type Pnt (LineSegment Point2 a) = Point2 a
  extractPoints (LS p p') = [p,p']
  endPoint (LS _ p') = p'


--------------------------------------------------------------------------------
-- Other instances


-- This drags in Undecidable Instances...

instance (Floating (Scalar (Diff (pt a))), 
          InnerSpace (Diff (pt a)), AffineSpace (pt a) )
    => Congruent (LineSegment pt a) where
  congruent l l' = segmentLength l == segmentLength l' 




-- | Reverse the direction of a line
instance Converse (LineSegment pt a) where
  converse (LS p p') = LS p' p

instance Converse (PolyLine pt a) where
  converse (PolyLine xs) = PolyLine $ reverse xs



--------------------------------------------------------------------------------

-- construction

-- | Line from vector.
line :: AffineSpace (pt a) => Diff (pt a) -> CoLineSegment pt a
line v = \p -> LS p (p .+^ v)

-- | Horizontal line from point @p@ of length @a@ .
hline :: (Num a, AffineSpace (pt a), HVec t, t a ~ Diff (pt a)) 
      => a -> CoLineSegment pt a
hline a = line (hvec a)

-- | Vertical line from point @p@ of length @a@.
vline :: (Num a, AffineSpace (pt a), VVec t, t a ~ Diff (pt a)) 
      => a -> CoLineSegment pt a
vline a = line (vvec a)


-- | A line from point @p@ in the direction @theta@ from x-axis
-- of length @a@
aline :: (Floating a, AffineSpace (pt a), Vec2 a ~ Diff (pt a)) 
      => Radian -> a -> CoLineSegment pt a
aline theta a = line (avec2 theta a)



lineTo :: pt a -> pt a -> LineSegment pt a
lineTo = LS --  p1 v where v = p2 .-. p1


--------------------------------------------------------------------------------
-- operations



-- | Angle ccw from x-axis
langle :: (Floating a, Real a) => LineSegment Point2 a -> Radian
langle (LS (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 



segmentLength :: (Floating (Scalar (Diff (pt a))), 
                  InnerSpace (Diff (pt a)), AffineSpace (pt a) )
              => LineSegment pt a -> Scalar (Diff (pt a))

segmentLength (LS p p') = distance p' p

lineCenter :: (Fractional (Scalar (Diff (pt a))), 
               AffineSpace (pt a), VectorSpace (Diff (pt a)))
           => LineSegment pt a -> pt a
lineCenter (LS p p') = midpoint p' p

-- | Expand line 
expandLine :: (Floating a, Real a, AffineSpace a, 
               InnerSpace a, a ~ Scalar a)
           => Scalar (Diff (Point2 a)) 
           -> LineSegment Point2 a 
           -> LineSegment Point2 a
expandLine n ln =  LS (p .-^ v) (p .+^ v) where
  v = avec2 (langle ln) (n*segmentLength ln/2)
  p = lineCenter ln