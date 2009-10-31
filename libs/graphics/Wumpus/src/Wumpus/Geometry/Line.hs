{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Line
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Lines - both line segments and lines in equational form.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.Line where

import Wumpus.Core.Geometry
import Wumpus.Geometry.Base

import Data.AffineSpace
import Data.VectorSpace

-- | Line in equational form, i.e. @Ax + By + C = 0@.
data Line u = Line !u !u !u 
  deriving (Eq,Show)

-- Would parametric form be more useful 
-- i.e. P2 .+^ (a)V2

-- | A straight line between 2 points.
data LineSegment u = LS2 (Point2 u) (Point2 u)
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Instances

instance Functor Line where
  fmap f (Line a b c) = Line (f a) (f b) (f c)

instance Functor LineSegment where
  fmap f (LS2 p0 p1) = LS2 (fmap f p0) (fmap f p1)



instance Num u => MatrixMult Matrix3'3 (LineSegment u) where
  type MatrixParam (LineSegment u) = u
  (*#) m3'3 (LS2 p0 p1) = LS2 (m3'3 *# p0) (m3'3 *# p1)



instance Pointwise (LineSegment u) where
  type Pt (LineSegment u) = Point2 u
  pointwise f (LS2 p0 p1) = LS2 (f p0) (f p1)


--------------------------------------------------------------------------------
-- construction

line :: Num u => Point2 u -> Point2 u -> Line u
line (P2 x1 y1) (P2 x2 y2) = Line a b c where
    a = y1 - y2
    b = x2 - x1
    c = (x1*y2) - (x2*y1)


lineSegment :: Point2 u -> Point2 u -> LineSegment u
lineSegment = LS2

toLine :: Num u => LineSegment u -> Line u
toLine (LS2 p0 p1) = line p0 p1



-- Lines are created /without/ respect to frames even though they 
-- are created at arbitrary points. A frame becomes necessary only 
-- later extraction of /points as coordinates/.

-- | Create a line with start point @p@ and end point @p .+^ v@.
dispLineSegment :: Num u => Vec2 u -> Point2 u -> LineSegment u
dispLineSegment v p = lineSegment p (p .+^ v)

-- | Horizontal line goinfg through point @p@.
hline :: Num u => Point2 u -> Line u
hline pt@(P2 x y) = line pt (P2 (-x) y)

-- | Horizontal line of length @a@ from point @p@.
hlineSegment :: Num u => u -> Point2 u -> LineSegment u
hlineSegment a = dispLineSegment (hvec a)


-- | Vertical line going through point @p@.
vline :: Num u => Point2 u -> Line u
vline pt@(P2 x y) = line pt (P2 x (-y))


-- | Vertical line segment of length @a@ from point @p@.
vlineSegment :: Num u => u -> Point2 u -> LineSegment u
vlineSegment a = dispLineSegment (vvec a)


-- | A line in the direction angle @theta@ from x-axis, going 
-- through point @p@.
aline :: Floating u => Radian -> Point2 u -> Line u
aline theta pt = line pt (pt .+^ avec theta 10)

-- | A line segment in the direction angle @theta@ from x-axis, of 
-- length @a@, starting at point @p@.
alineSegment :: Floating u => Radian -> u -> Point2 u -> LineSegment u
alineSegment theta a = dispLineSegment (avec theta a)




--------------------------------------------------------------------------------
-- operations


-- Reverse the direction of a line segment.

instance Converse (LineSegment u) where
  converse (LS2 p0 p1) = LS2 p1 p0

slope :: (Fractional u, Real u) => Line u -> u
slope (Line a b _) = (-a)/b 

class CCWAngle a where ccwAngle :: a -> Radian

instance (Floating u, Real u) => CCWAngle (Line u) where
  ccwAngle = atan . toRadian . slope

instance (Floating u, Real u) => CCWAngle (LineSegment u) where
  ccwAngle (LS2 (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 


-- | Line segment length.
lsLength :: (Floating u, InnerSpace (Vec2 u)) => LineSegment u -> u
lsLength (LS2 p0 p1) = distance p1 p0



-- | Midpoint between two points.
midpoint :: (Fractional a, AffineSpace p, VectorSpace v, Diff p ~ v, Scalar v ~ a)
         => p -> p -> p
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


-- | Line segment midpoint.
lsMidpoint :: Fractional u => LineSegment u -> Point2 u
lsMidpoint (LS2 p0 p1) = midpoint p0 p1


-- | Expand line segment.
expandLineSegment :: (Floating u, Real u, InnerSpace (Vec2 u)) 
                  => u -> LineSegment u -> LineSegment u
expandLineSegment n l =  LS2 (p .-^ v) (p .+^ v) where
    v = avec (ccwAngle l) (n*lsLength l/2)
    p = lsMidpoint l



