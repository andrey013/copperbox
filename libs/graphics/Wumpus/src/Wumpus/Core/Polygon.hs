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

  , BoundingBox(..)
  , DBoundingBox


  -- * Construction
  , regularPolygon
  , squareAt
  , square
  , rectangleAt
  , rectangle
  , isoscelesTriangleAt
  , isoscelesTriangle

  -- * Predicates
  , simplePolygon
  , concavePolygon

  -- * Operations
  , interiorAngles
  , boundingBox

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

-- | Bounding box, two point representation (bottom-left and top-right). 
data BoundingBox a = BBox { bbBottomLeft :: Point2 a, bbTopRight :: Point2 a }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double





instance Pointwise (Polygon a) where
  type Pt (Polygon a) = (Point2 a)
  pointwise f (Polygon xs) = Polygon $ map f xs




--------------------------------------------------------------------------------
-- Construction


-- | Create a regular polgon with @n@ sides, and displacement @vec@ from the
-- origin for the first point.

regularPolygon :: (AffineSpace a, Floating a)
               => Int -> Diff (Point2 a) -> Polygon a
regularPolygon n vec = Polygon ps
  where 
    ps = circular $ replicate n (zeroPt .+^ vec) 


-- | Create a square with bottom-left corner @p@ and side-length @d@.
squareAt :: (Num a, AffineSpace a) => Point2 a -> a -> Polygon a
squareAt p d = Polygon $ [p,p2,p3,p4] where
  p2 = p  .+^ hvec d
  p3 = p2 .+^ vvec d
  p4 = p3 .+^ (hvec $ negate d)
  

-- | Create a square with side-length d. Note bottom-left corner is at 
-- coordinate (0,0).
square :: (Num a, AffineSpace a) => a -> Polygon a
square = squareAt zeroPt

-- | Create a rectangle with bottom-left corner @p@ and width @w@ and
-- height @h@.
rectangleAt :: (Num a, AffineSpace a) => Point2 a -> a -> a -> Polygon a
rectangleAt p w h = Polygon $ [p,p2,p3,p4] where
  p2 = p  .+^ hvec w
  p3 = p2 .+^ vvec h
  p4 = p3 .+^ (hvec $ negate w)
  
-- | Create a rectangle of width @w@ and height @h@. 
-- Note bottom-left corner is at coordinate (0,0).
rectangle :: (Num a, AffineSpace a) => a -> a -> Polygon a
rectangle = rectangleAt zeroPt

-- | Create an isosceles rectangle with bottom-left corner @p@, the base 
-- in on the horizontal plane with width @bw@. Height is @h@.
isoscelesTriangleAt :: (Fractional a, AffineSpace a) 
                    => Point2 a -> a -> a -> Polygon a
isoscelesTriangleAt p bw h = Polygon [p,p2,p3] where
  p2 = p .+^ hvec bw
  p3 = p .+^ V2 (bw/2) h  

-- | Create a isosceles triangle with base width @w@ and height @h@. 
-- Note bottom-left corner is at coordinate (0,0).
isoscelesTriangle :: (Fractional a, AffineSpace a) => a -> a -> Polygon a
isoscelesTriangle = isoscelesTriangleAt zeroPt

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

-- | Extract the interior angles of polygon.
interiorAngles :: (a ~ Scalar a, Floating a, AffineSpace a, InnerSpace a)
               => Polygon a -> [Radian a]
interiorAngles (Polygon ps) = windowedMap3c intAng ps where
  intAng a b c = vangle (a .-. b) (c .-. b)

-- | Calculate the bounding box of a polygon.
boundingBox :: Ord a => Polygon a -> BoundingBox a
boundingBox (Polygon (p:ps)) = uncurry BBox $ foldr fn (p,p) ps
  where
    fn (P2 x y) (P2 xmin ymin, P2 xmax ymax) = 
       (P2 (min x xmin) (min y ymin), P2 (max x xmax) (max y ymax))
boundingBox (Polygon _)      = error $ "boundingBox: degenerate polygon"


