{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Polygon
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Polygon
--
--------------------------------------------------------------------------------


module Wumpus.Core.Polygon
  (
  -- * Polygon types
    Polygon(..)
  , DPolygon
  -- * Construction
  , regularPolygon

  -- * Predicates
  , simplePolygon
  , concavePolygon

  -- * Operations
  , interiorAngles

  ) where

import Wumpus.Core.Fun
import Wumpus.Core.Instances ()
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

import Data.List ( nub )

--------------------------------------------------------------------------------
-- Polygon types and standard instances


data Polygon a = Polygon [Point2 a]
  deriving (Eq,Show)


type DPolygon = Polygon Double

instance Pointwise (Polygon a) where
  type Pt (Polygon a) = (Point2 a)
  pointwise f (Polygon xs) = Polygon $ map f xs




--------------------------------------------------------------------------------
-- Construction


-- | Draw a regular polgon with @n@ sides, and displacement @vec@ from the
-- origin for the first point.

regularPolygon :: (AffineSpace a, Floating a)
               => Int -> Diff (Point2 a) -> Polygon a
regularPolygon n vec = Polygon ps
  where 
    ps = circular $ replicate n (P2 0 0 .+^ vec) 




--------------------------------------------------------------------------------
-- predicates

-- | This definition is not satisfactory...
simplePolygon :: Eq a => Polygon a -> Bool
simplePolygon (Polygon ps) 
   | length ps >= 2 = total_len == length (nub ps)
   | otherwise      = error $ "simplePolygon: malformed too few points"
  where
    total_len       = (length ps) - consecutive_pts
    consecutive_pts = windowedFoldR2c fn 0 ps
    fn p p' n       | p==p'     = n+1
                    | otherwise = n
             

-- | A polygon is concave if at least 1 interior angle is greater then pi/2.
-- concavePolygon :: (Ord a, Floating a) => Polygon a -> Bool
concavePolygon :: (a ~ Scalar a, Ord a, Floating a, AffineSpace a, InnerSpace a)
               => Polygon a -> Bool
concavePolygon = any (>pi/2) . interiorAngles



--------------------------------------------------------------------------------
-- Operations

-- Extract the interior angles.

interiorAngles :: (a ~ Scalar a, Floating a, AffineSpace a, InnerSpace a)
               => Polygon a -> [Radian a]
interiorAngles (Polygon ps) = windowedMap3c intAng ps where
  intAng a b c = vangle (a .-. b) (c .-. b)
