{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.LineSegment
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Line segments.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.LineSegment 
  (
  -- * Data types
    LineSegment(..)
  , DLineSegment

  -- * Contruction
  , lineSegment
  , lineSegmentV
  , hlineSegment
  , vlineSegment
  , alineSegment
  , hlineSegmentBisect
  , vlineSegmentBisect

  , expandLineSegment
  , lineSegmentToPath
  
  ) where

import Wumpus.Geometry.Base


import Wumpus.Core

import Data.AffineSpace
import Data.VectorSpace

-- | A straight line between 2 points.
data LineSegment u = LS2 (Point2 u) (Point2 u)
  deriving (Eq,Show)

type DLineSegment = LineSegment Double


--------------------------------------------------------------------------------
-- Instances

type instance DUnit (LineSegment u) = u


instance Functor LineSegment where
  fmap f (LS2 p0 p1) = LS2 (fmap f p0) (fmap f p1)



instance Num u => MatrixMult (LineSegment u) where
  (*#) m3'3 (LS2 p0 p1) = LS2 (m3'3 *# p0) (m3'3 *# p1)



instance Pointwise (LineSegment u) where
  type Pt (LineSegment u) = Point2 u
  pointwise f (LS2 p0 p1) = LS2 (f p0) (f p1)


--------------------------------------------------------------------------------
-- Affine instances

instance (Floating u, Real u) => Rotate (LineSegment u) where
  rotate ang = pointwise (rotate ang) 


instance (Floating u, Real u) => RotateAbout (LineSegment u) where
  rotateAbout r pt = pointwise (rotateAbout r pt) 

instance (Floating u, Real u) => Scale (LineSegment u) where
  scale x y = pointwise (scale x y) 


instance (Floating u, Real u) => Translate (LineSegment u) where
  translate x y = pointwise (translate x y) 


--------------------------------------------------------------------------------
-- Geometry instances

-- Reverse the direction of a line segment.

instance Converse (LineSegment u) where
  converse (LS2 p0 p1) = LS2 p1 p0

instance (Floating u, Real u) => CCWAngle (LineSegment u) where
  ccwAngle (LS2 (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 

instance (Floating u, InnerSpace v, v ~ Vec2 u) => 
    ObjectLength (LineSegment u) where
  objectLength (LS2 p0 p1) = distance p1 p0

instance Fractional u => Midpoint (LineSegment u) where
  midpoint (LS2 p0 p1) = midpointBetween p0 p1


--------------------------------------------------------------------------------
-- construction


lineSegment :: Point2 u -> Point2 u -> LineSegment u
lineSegment = LS2


-- | Create a line with start point @p@ and end point @p .+^ v@.
lineSegmentV :: Num u => Vec2 u -> Point2 u -> LineSegment u
lineSegmentV v p = lineSegment p (p .+^ v)


-- | Horizontal line of length @a@ from point @p@.
hlineSegment :: Num u => u -> Point2 u -> LineSegment u
hlineSegment a = lineSegmentV (hvec a)


-- | Vertical line segment of length @a@ from point @p@.
vlineSegment :: Num u => u -> Point2 u -> LineSegment u
vlineSegment a = lineSegmentV (vvec a)


-- | A line segment in the direction angle @theta@ from x-axis, of 
-- length @a@, starting at point @p@.
alineSegment :: Floating u => Radian -> u -> Point2 u -> LineSegment u
alineSegment theta a = lineSegmentV (avec theta a)



-- | Horizontal line of length 2x@a@ centered at point @p@.
hlineSegmentBisect :: Num u => u -> Point2 u -> LineSegment u
hlineSegmentBisect hl (P2 x y) = LS2 (P2 (x-hl) y) (P2 (x+hl) y)

-- | Vertical line of length 2x@a@ centered at point @p@.
vlineSegmentBisect :: Num u => u -> Point2 u -> LineSegment u
vlineSegmentBisect hl (P2 x y) = LS2 (P2 x (y-hl)) (P2 x (y+hl))



--------------------------------------------------------------------------------
-- operations



-- | Expand line segment.
expandLineSegment :: (Floating u, Real u, InnerSpace v, v ~ Vec2 u) 
                  => u -> LineSegment u -> LineSegment u
expandLineSegment n line =  LS2 (p .-^ v) (p .+^ v) where
    l = objectLength line
    v = avec (ccwAngle line) (l * 0.5 * n)
    p = midpoint line



--------------------------------------------------------------------------------
-- To picture types

lineSegmentToPath :: LineSegment u -> Path u
lineSegmentToPath (LS2 p1 p2) = vertexPath [p1,p2]