{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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
  , CoPolygon
  , DCoPolygon

  , BoundingBox(..)
  , DBoundingBox


  -- * Construction
  , regularPolygon
  , square
  , rectangle
  , isoscelesTriangle

  -- * Predicates
  , simplePolygon
  , concavePolygon

  -- * Operations
  , interiorAngles
  , boundingBox
  , topLeftBottomRight
  , bottomLeft
  , bottomRight
  , topLeft
  , topRight
  ) where

import Wumpus.Core.Fun
import Wumpus.Core.Geometric
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


type CoPolygon a = Point2 a -> Polygon a
type DCoPolygon  = CoPolygon Double



instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs


instance Pointwise (CoPolygon a) where
  type Pt (CoPolygon a) = Point2 a
  pointwise f pf = pf . f

instance ExtractPoints (Polygon a) where
  type Pnt (Polygon a) = Point2 a
  extractPoints (Polygon xs) = xs
  endPoint (Polygon (x:_))   = x        -- start point is also end point   
  endPoint (Polygon _)       = error "endPoint: malformed Polygon, too few points"

instance ExtractPoints (BoundingBox a) where
  type Pnt (BoundingBox a) = Point2 a
  extractPoints (BBox p1@(P2 xmin ymin) p2@(P2 xmax ymax)) = [p1,br,p2,tl]
    where br = P2 xmax ymin
          tl = P2 xmin ymax
  endPoint (BBox start _) = start       -- start point is also end point   
                
--------------------------------------------------------------------------------
-- Construction

-- NOTE - supplying the displacement vector to regularPolygon /feels/ wrong.
-- This function needs a re-think.

-- | Create a regular polgon with @n@ sides, and displacement @vec@ from the
-- centre for the first point.
regularPolygon :: (Floating a, Real a, AffineSpace a)
               => Int -> a -> CoPolygon a
regularPolygon n radius = Polygon . pf
  where 
    pf = \pt -> circular $ replicate n (pt .+^ (V2 0 radius)) 


-- Note square and rectangle are both 'turtle drawn' and use the @iter@ 
-- functional to successively transform the current point.

-- | Create a square with bottom-left corner @p@ and side-length @d@.
square :: (Num a, AffineSpace a) => a -> CoPolygon a
square d = Polygon . iter [id,f2,f3,f4] where
  f2 = (.+^ hvec d)
  f3 = (.+^ vvec d)
  f4 = (.+^ (hvec $ negate d))
  

-- | Create a rectangle with bottom-left corner @p@ and width @w@ and
-- height @h@.
rectangle :: (Num a, AffineSpace a) => a -> a -> CoPolygon a
rectangle w h = Polygon . iter [id,f2,f3,f4] where
  f2 = (.+^ hvec w)
  f3 = (.+^ vvec h)
  f4 = (.+^ (hvec $ negate w))


-- | Create an isosceles rectangle with bottom-left corner @p@, the base 
-- in on the horizontal plane with width @bw@. Height is @h@.
isoscelesTriangle :: (Fractional a, AffineSpace a) 
                  => a -> a -> CoPolygon a
isoscelesTriangle bw h = Polygon . sequence [id,f2,f3] where
  f2 = (.+^ hvec bw)
  f3 = (.+^ V2 (bw/2) h)




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
concavePolygon :: (Ord a, Floating a, Real a, 
                   AffineSpace a, InnerSpace a, a ~ Scalar a)
               => Polygon a -> Bool
concavePolygon = any (>pi/2) . interiorAngles



--------------------------------------------------------------------------------
-- Operations

-- | Extract the interior angles of polygon.
interiorAngles :: (a ~ Scalar a, Floating a, Real a, 
                   AffineSpace a, InnerSpace a)
               => Polygon a -> [Radian]
interiorAngles (Polygon ps) = windowedMap3c intAng ps where
  intAng a b c = interiorAngle (a .-. b) (c .-. b)

-- | Calculate the bounding box of a polygon.
boundingBox :: Ord a => Polygon a -> BoundingBox a
boundingBox (Polygon (p:ps)) = uncurry BBox $ foldr fn (p,p) ps
  where
    fn (P2 x y) (P2 xmin ymin, P2 xmax ymax) = 
       (P2 (min x xmin) (min y ymin), P2 (max x xmax) (max y ymax))
boundingBox (Polygon _)      = error $ "boundingBox: degenerate polygon"


-- | Extract the opposite corners (tl,br) of a bounding box.
topLeftBottomRight :: BoundingBox a -> (Point2 a,Point2 a)
topLeftBottomRight (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
  (P2 xmin ymax, P2 xmax ymin) 

-- | bl of bounding box.
bottomLeft  :: BoundingBox a -> Point2 a
bottomLeft  = bbBottomLeft

-- | br of bounding box.
bottomRight :: BoundingBox a -> Point2 a
bottomRight (BBox (P2 _ y) (P2 x _)) = P2 x y

-- | tl of bounding box.
topLeft     :: BoundingBox a -> Point2 a
topLeft (BBox (P2 x _) (P2 _ y))     = P2 x y
 
-- | tr of bounding box.
topRight    :: BoundingBox a -> Point2 a
topRight    = bbTopRight



