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
-- Lines - straight lines.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.Line where

import Wumpus.Core.Geometry

import Data.AffineSpace
import Data.VectorSpace




-- | A stright line between 2 points.
data Line u = Line (Point2 u) (Point2 u)
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Instances

instance Functor Line where
  fmap f (Line p0 p1) = Line (fmap f p0) (fmap f p1)



instance Num u => MatrixMult Matrix3'3 (Line u) where
  type MatrixParam (Line u) = u
  (*#) m3'3 (Line p0 p1) = Line (m3'3 *# p0) (m3'3 *# p1)



instance Pointwise (Line u) where
  type Pt (Line u) = Point2 u
  pointwise f (Line p0 p1) = Line (f p0) (f p1)


--------------------------------------------------------------------------------
-- construction

line :: Point2 u -> Point2 u -> Line u
line = Line

-- Lines are created /without/ respect to frames even though they 
-- are created at arbitrary points. A frame becomes necessary only 
-- later extraction of /points as coordinates/.

-- | Create a line with start point @p@ and end point @p .+^ v@.
dispLine :: Num u => Vec2 u -> Point2 u -> Line u
dispLine v p = line p (p .+^ v)


-- | Horizontal line of length @a@ from point @p@.
hline :: Num u => u -> Point2 u -> Line u
hline a = dispLine (hvec a)


-- | Vertical line of length @a@ from point @p@.
vline :: Num u => u -> Point2 u -> Line u
vline a = dispLine (vvec a)


-- | A line in the direction angle @theta@ from x-axis, of 
-- length @a@, starting at point @p@
aline :: Floating u => Radian -> u -> Point2 u -> Line u
aline theta a = dispLine (avec theta a)




--------------------------------------------------------------------------------
-- operations


-- | Reverse the direction of a line
converse :: Line u -> Line u
converse (Line p0 p1) = Line p1 p0


ccwAngle :: (Floating u, Real u) => Line u -> Radian
ccwAngle (Line (P2 x y) (P2 x' y')) = toRadian $ atan $ (y'-y) / (x'-x) 



lineLength :: (Floating u, InnerSpace (Vec2 u)) => Line u -> u
lineLength (Line p0 p1) = distance p1 p0



-- | midpoint between two points
midpoint :: (Fractional a, AffineSpace p, VectorSpace v, Diff p ~ v, Scalar v ~ a)
         => p -> p -> p
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0



lineMidpoint :: Fractional u => Line u -> Point2 u
lineMidpoint (Line p0 p1) = midpoint p0 p1


-- | Expand line 
expandLine :: (Floating u, Real u, InnerSpace (Vec2 u)) => u -> Line u -> Line u
expandLine n ln =  Line (p .-^ v) (p .+^ v) 
  where
    v = avec (ccwAngle ln) (n*lineLength ln/2)
    p = lineMidpoint ln



