{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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

  -- ** Poly lines
  , PolyLine(..)
  , DPolyLine2

  -- * Construction
  , lineSegment
  , line
  , hline
  , vline
  , aline


  -- * Operations
  , converse

  , LineAngle(..)

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


data LineSegment pt = LS pt pt
  deriving (Eq,Show)

-- LineSegment in 2-space.
type DLineSegment2 = LineSegment (Point2 Double)



-- Poly Lines

data PolyLine pt = PolyLine [pt]
  deriving (Eq,Show)

-- | PolyLine in 2-space.
type DPolyLine2 = PolyLine (Point2 Double)



instance Functor LineSegment where
  fmap f (LS p p') = LS (f p) (f p')

{-
instance MatrixMult Matrix3'3 LineSegment  where
  (*#) m3'3 (LS p p') = LS (m3'3 *# p) (m3'3 *# p')
-}


instance Functor PolyLine where
  fmap f (PolyLine ps) = PolyLine (map f ps)


{-
instance MatrixMult Matrix3'3 (PolyLine Point2) where
  (*#) m3'3 (PolyLine ps) = PolyLine (map (m3'3 *#) ps)
-}

instance Pointwise (LineSegment pt) where
  type Pt (LineSegment pt) = pt
  pointwise f (LS p p') = LS (f p) (f p')

instance Pointwise (PolyLine pt) where
  type Pt (PolyLine pt) = pt
  pointwise f (PolyLine ps) = PolyLine (map f ps)


instance HasPoints (LineSegment pt) where
  type Pnt (LineSegment pt) = pt
  extractPoints (LS p p') = [p,p']
  endPoint (LS _ p')      = p'
  startPoint (LS p _)     = p

--------------------------------------------------------------------------------
-- Other instances


-- This drags in Undecidable Instances...

instance (Floating a, InnerSpace v, AffineSpace pt,
          Diff pt ~ v, Scalar v ~ a)
    => Congruent (LineSegment pt) where
  congruent l l' = segmentLength l == segmentLength l' 



-- | Reverse the direction of a line
instance Converse (LineSegment pt) where
  converse (LS p p') = LS p' p

instance Converse (PolyLine pt) where
  converse (PolyLine xs) = PolyLine $ reverse xs



--------------------------------------------------------------------------------

-- construction


lineSegment :: pt -> pt -> LineSegment pt
lineSegment = LS

-- Lines are created /without/ respect to frames even though they 
-- are created at arbitrary points. A frame becomes necessary only 
-- later extraction of /points as coordinates/.

-- | Line from vector starting from Point.
line :: (AffineSpace pt, Diff pt ~ v) 
     => v -> pt -> LineSegment pt
line v p = LS p (p .+^ v)


-- | Horizontal line from point @p@ of length @a@ .
hline :: (Num a, AffineSpace pt, HVec v, Diff pt ~ v, Scalar v ~ a)
      => a -> (pt -> LineSegment pt)
hline a = line (hvec a)

-- | Vertical line from point @p@ of length @a@.
vline :: (Num a, AffineSpace pt, VVec v, Diff pt ~ v, Scalar v ~ a) 
       => a -> (pt -> LineSegment pt)
vline a = line (vvec a)


-- | A line from point @p@ in the direction @theta@ from x-axis
-- of length @a@
aline :: (Floating a, AffineSpace pt,
          Diff pt ~ v, Scalar v ~ a, v ~ Vec2 a)
      => Radian -> a -> (pt -> LineSegment pt)
aline theta a = line (avec2 theta a)




--------------------------------------------------------------------------------
-- operations



-- | Angle ccw from x-axis
class LineAngle pt where
  langle :: LineSegment pt -> Radian

instance (Floating a, Real a) => LineAngle (Point2 a) where
  langle (LS (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 


-- langle :: (Floating a, Real a) => LineSegment (Point2 a) -> Radian
-- langle (LS (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 


segmentLength :: (Floating a, AffineSpace pt, InnerSpace v,
                  Diff pt ~ v, Scalar v ~ a)
              => LineSegment pt -> a    
segmentLength (LS p p') = distance p' p

lineCenter :: (Fractional a, AffineSpace pt, VectorSpace v,
               Diff pt ~ v, Scalar v ~ a)
           => LineSegment pt -> pt
lineCenter (LS p p') = midpoint p' p

-- | Expand line 
expandLine :: (Floating a, Real a, LineAngle pt, AffineSpace pt, InnerSpace v,
               v ~ Vec2 a, Diff pt ~ v, Scalar v ~ a)
           => a 
           -> LineSegment pt
           -> LineSegment pt
expandLine n ln =  LS (p .-^ v) (p .+^ v) 
  where
    v = avec2 (langle ln) (n*segmentLength ln/2)
    p = lineCenter ln