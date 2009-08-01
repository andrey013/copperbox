{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.BoundingBox
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- BoundingBox
--
--------------------------------------------------------------------------------


module Wumpus.Core.BoundingBox
  (
  -- * Bounding box types
    BoundingBox(..)
  , DBoundingBox

  -- * Operations
  , boundingBox
  , bounds
  , within
  , width 
  , height
  , oppositeCorners

  , center
  , north
  , south
  , east
  , west

  , northEast
  , southEast
  , northWest
  , southWest
  
  , centeredAt
  
  ) where

import Wumpus.Core.Geometric
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Vector

import Data.AffineSpace

import Data.Monoid

--------------------------------------------------------------------------------
-- Polygon types and standard instances


-- | Bounding box, two point representation (bottom-left and top-right). 
data BoundingBox a = BBox { bbBottomLeft :: Point2 a, bbTopRight :: Point2 a }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double


instance HasPoints (BoundingBox a) where
  type Pnt (BoundingBox a) = Point2 a
  extractPoints (BBox p1@(P2 xmin ymin) p2@(P2 xmax ymax)) = [p1,br,p2,tl]
    where br = P2 xmax ymin
          tl = P2 xmin ymax
  endPoint (BBox start _) = start       -- start point is also end point   
  startPoint              = endPoint  

instance (Fractional a, Ord a) => Monoid (BoundingBox a) where
  mempty  = BBox (P2 inf inf) (P2 (-inf) (-inf))  where inf = 1/0
  mappend = bbProd
                
--------------------------------------------------------------------------------
-- Construction

-- | Calculate the bounding box of a polygon.
boundingBox :: Ord a => Polygon a -> BoundingBox a
boundingBox (Polygon ps) = bounds' ps


bounds :: (HasPoints t, Ord a, Pnt t ~ Point2 a) 
       => t -> BoundingBox a
bounds = bounds' . extractPoints

bounds' :: Ord a => [Point2 a] -> BoundingBox a
bounds' []     = error $ "Polygon.bounds' - empty list"
bounds' (p:ps) = uncurry BBox $ foldr fn (p,p) ps
  where
    fn (P2 x y) (P2 xmin ymin, P2 xmax ymax) = 
       (P2 (min x xmin) (min y ymin), P2 (max x xmax) (max y ymax))


--------------------------------------------------------------------------------
-- Operations



bbProd :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
bbProd (BBox (P2 xmin1 ymin1) (P2 xmax1 ymax1))
       (BBox (P2 xmin2 ymin2) (P2 xmax2 ymax2))
  = BBox (P2 (min xmin1 xmin2) (min ymin1 ymin2)) 
         (P2 (max xmax1 xmax2) (max ymax1 ymax2))



within :: Ord a => Point2 a -> BoundingBox a -> Bool
within (P2 x y) (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
   x >= xmin && x <= xmax && y >= ymin && y <= ymax


width :: Num a => BoundingBox a -> a
width (BBox (P2 xmin _) (P2 xmax _)) = xmin + (xmax-xmin)


height :: Num a => BoundingBox a -> a
height (BBox (P2 _ ymin) (P2 _ ymax)) = ymin + (ymax-ymin)


-- | Extract the opposite corners (tl,br) of a bounding box.
oppositeCorners :: BoundingBox a -> (Point2 a,Point2 a)
oppositeCorners (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
  (P2 xmin ymax, P2 xmax ymin) 

--------------------------------------------------------------------------------
-- Points on a bounding box



-- Center (middle point) of a bounding box.
center :: Fractional a => BoundingBox a -> Point2 a
center (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
    P2 (xmin + (0.5*(xmax-xmin))) (ymin + (0.5*(ymax-ymin))) 

-- @north@ - middle point on the top of a bounding box.
north :: Fractional a => BoundingBox a -> Point2 a
north (BBox (P2 xmin _) (P2 xmax ymax)) = P2 (xmin + 0.5*(xmax-xmin)) ymax

-- @south@ - middle point on the bottom of a bounding box.
south :: Fractional a => BoundingBox a -> Point2 a
south (BBox (P2 xmin ymin) (P2 xmax _)) = P2 (xmin + 0.5*(xmax-xmin)) ymin



-- @east@ - middle point on the right side of the bounding box.
east :: Fractional a => BoundingBox a -> Point2 a
east (BBox (P2 _ ymin) (P2 xmax ymax)) = P2 xmax (ymin + 0.5*(ymax-ymin))

-- @west@ - middle point on the left side of the bounding box.
west :: Fractional a => BoundingBox a -> Point2 a
west (BBox (P2 xmin ymin) (P2 _ ymax)) = P2 xmin (ymin + 0.5*(ymax-ymin))

 
-- | @northEast@ top-right corner of the bounding box.
northEast :: BoundingBox a -> Point2 a
northEast = bbTopRight


-- | @southEast@ - bottom-right corner of the bounding box.
southEast :: BoundingBox a -> Point2 a
southEast (BBox (P2 _ y) (P2 x _)) = P2 x y


-- | @northWest@ - top-left corner of the bounding box.
northWest :: BoundingBox a -> Point2 a
northWest (BBox (P2 x _) (P2 _ y))     = P2 x y
 
-- | @southWest@ - bottom-left corner of the bounding box.
southWest :: BoundingBox a -> Point2 a
southWest = bbBottomLeft


-- | Center a shape at the supplied point.

centeredAt :: (Fractional a, Ord a, HasPoints shape, Pointwise shape, 
               AffineSpace (Pt shape), 
               Pnt shape ~ Point2 a, Diff (Pt shape) ~ Vec2 a) 
           => shape -> Point2 a -> shape
centeredAt sh pt = pointwise (.+^ diff) sh
  where
    diff = pt .-. center (bounds sh) 




