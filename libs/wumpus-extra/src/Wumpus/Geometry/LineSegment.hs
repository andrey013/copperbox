{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.LineSegment
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Line segments.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.LineSegment where

import Wumpus.Geometry.Base


import Wumpus.Core

import Data.AffineSpace
import Data.VectorSpace

-- | A straight line between 2 points.
data LineSegment u = LS2 (Point2 u) (Point2 u)
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Instances

instance Functor LineSegment where
  fmap f (LS2 p0 p1) = LS2 (fmap f p0) (fmap f p1)



instance Num u => MatrixMult Matrix3'3 (LineSegment u) where
  type MatrixParam (LineSegment u) = u
  (*#) m3'3 (LS2 p0 p1) = LS2 (m3'3 *# p0) (m3'3 *# p1)



instance Pointwise (LineSegment u) where
  type Pt (LineSegment u) = Point2 u
  pointwise f (LS2 p0 p1) = LS2 (f p0) (f p1)


--------------------------------------------------------------------------------
-- Affine instances

instance (Floating u, Real u) => Rotate (LineSegment u) where
  rotate ang = pointwise (rotate ang) 


instance (Floating u, Real u) => RotateAbout (LineSegment u) where
  type RotateAboutUnit (LineSegment u) = u
  rotateAbout r pt = pointwise (rotateAbout r pt) 

instance (Floating u, Real u) => Scale (LineSegment u) where
  type ScaleUnit (LineSegment u) = u
  scale x y = pointwise (scale x y) 


instance (Floating u, Real u) => Translate (LineSegment u) where
  type TranslateUnit (LineSegment u) = u
  translate x y = pointwise (translate x y) 


--------------------------------------------------------------------------------
-- Geometry instances

-- Reverse the direction of a line segment.

instance Converse (LineSegment u) where
  converse (LS2 p0 p1) = LS2 p1 p0

instance (Floating u, Real u) => CCWAngle (LineSegment u) where
  ccwAngle (LS2 (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 


--------------------------------------------------------------------------------
-- construction


lineSegment :: Point2 u -> Point2 u -> LineSegment u
lineSegment = LS2


-- | Create a line with start point @p@ and end point @p .+^ v@.
dispLineSegment :: Num u => Vec2 u -> Point2 u -> LineSegment u
dispLineSegment v p = lineSegment p (p .+^ v)


-- | Horizontal line of length @a@ from point @p@.
hlineSegment :: Num u => u -> Point2 u -> LineSegment u
hlineSegment a = dispLineSegment (hvec a)


-- | Vertical line segment of length @a@ from point @p@.
vlineSegment :: Num u => u -> Point2 u -> LineSegment u
vlineSegment a = dispLineSegment (vvec a)


-- | A line segment in the direction angle @theta@ from x-axis, of 
-- length @a@, starting at point @p@.
alineSegment :: Floating u => Radian -> u -> Point2 u -> LineSegment u
alineSegment theta a = dispLineSegment (avec theta a)




--------------------------------------------------------------------------------
-- operations



-- | Line segment length.
lsLength :: (Floating u, InnerSpace (Vec2 u)) => LineSegment u -> u
lsLength (LS2 p0 p1) = distance p1 p0


-- | Line segment midpoint.
lsMidpoint :: Fractional u => LineSegment u -> Point2 u
lsMidpoint (LS2 p0 p1) = midpoint p0 p1


-- | Expand line segment.
expandLineSegment :: (Floating u, Real u, InnerSpace (Vec2 u)) 
                  => u -> LineSegment u -> LineSegment u
expandLineSegment n l =  LS2 (p .-^ v) (p .+^ v) where
    v = avec (ccwAngle l) (n*lsLength l/2)
    p = lsMidpoint l



--------------------------------------------------------------------------------
-- To picture types

lineSegmentToPath :: LineSegment u -> Path u
lineSegmentToPath (LS2 p1 p2) = vertexPath [p1,p2]