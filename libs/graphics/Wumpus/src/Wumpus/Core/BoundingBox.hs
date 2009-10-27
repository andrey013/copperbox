{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.BoundingBox
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.BoundingBox where

import Wumpus.Core.Geometry

import Data.Groupoid

import Data.List ( foldl' )


data BoundingBox a = BBox { bottomLeft :: Point2 a, topRight :: Point2 a }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double



--------------------------------------------------------------------------------

union :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
union (BBox pmin pmax) (BBox pmin' pmax') = 
    BBox (umin pmin pmin') (umax pmax pmax')
  where
    umin (P2 x y) (P2 x' y') = P2 (min x x') (min y y')
    umax (P2 x y) (P2 x' y') = P2 (max x x') (max y y')


-- We don't consider BBox to have a (nice) zero, hence 
-- the Groupoid instance rather than a Monoid instance.

instance Ord a => Groupoid (BoundingBox a) where
  gappend = union

instance Pointwise (BoundingBox a) where
  type Pt (BoundingBox a) = Point2 a
  pointwise f (BBox bl tr) = BBox (f bl) (f tr)


--------------------------------------------------------------------------------


-- Trace the point list finding the /extremity/...

trace :: (Num a, Ord a) => [Point2 a] -> BoundingBox a
trace []     = error $ "BoundingBox.trace - empty list"
trace (p:ps) = foldl' fn (BBox p p) ps
  where
    fn (BBox (P2 xmin ymin) (P2 xmax ymax)) (P2 x y) = 
        BBox (P2 (min xmin x) (min ymin y)) (P2 (max xmax x) (max ymax y))


corners :: BoundingBox a -> [Point2 a]
corners (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = [bl, br, tr, tl] where
    br = P2 x1 y0
    tl = P2 x0 y1


within :: Ord a => Point2 a -> BoundingBox a -> Bool
within (P2 x y) (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
   x >= xmin && x <= xmax && y >= ymin && y <= ymax


width :: Num a => BoundingBox a -> a
width (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

height :: Num a => BoundingBox a -> a
height (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin


--------------------------------------------------------------------------------

-- points on the boundary

topLeft :: BoundingBox a -> Point2 a
topLeft (BBox (P2 xmin _) (P2 _ ymax)) = P2 xmin ymax

bottomRight :: BoundingBox a -> Point2 a
bottomRight (BBox (P2 _ ymin) (P2 xmax _)) = P2 xmax ymin

center :: Fractional a => BoundingBox a -> Point2 a
center (BBox (P2 x y) (P2 x' y')) = P2 (x+0.5*(x'-x)) (y+0.5*(y'-x))

north :: Fractional a => BoundingBox a -> Point2 a
north (BBox (P2 xmin _) (P2 xmax ymax)) = P2 (xmin + 0.5*(xmax-xmin)) ymax

south :: Fractional a => BoundingBox a -> Point2 a
south (BBox (P2 xmin ymin) (P2 xmax _)) = P2 (xmin + 0.5*(xmax-xmin)) ymin

east :: Fractional a => BoundingBox a -> Point2 a
east (BBox (P2 _ ymin) (P2 xmax ymax)) = P2 xmax (ymin + 0.5*(ymax-ymin))

west :: Fractional a => BoundingBox a -> Point2 a
west (BBox (P2 xmin ymin) (P2 _ ymax)) = P2 xmin (ymin + 0.5*(ymax-ymin))

northEast :: BoundingBox a -> Point2 a
southEast :: BoundingBox a -> Point2 a
southWest :: BoundingBox a -> Point2 a
northWest :: BoundingBox a -> Point2 a

northEast = topRight
southEast = bottomRight
southWest = bottomLeft
northWest = topLeft 

--------------------------------------------------------------------------------

-- /planes/ on the bounding box

leftPlane :: BoundingBox a -> a
leftPlane (BBox (P2 l _) _) = l

rightPlane :: BoundingBox a -> a
rightPlane (BBox _ (P2 r _)) = r

lowerPlane :: BoundingBox a -> a
lowerPlane (BBox (P2 _ l) _) = l

upperPlane :: BoundingBox a -> a
upperPlane (BBox _ (P2 _ u)) = u






