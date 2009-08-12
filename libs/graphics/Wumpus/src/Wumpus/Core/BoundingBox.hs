{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

  , HasBoundingBox(..)

  -- * Operations
  , bounds
  , within
  , width 
  , height
  , oppositeCorners

  , ReferencePoints(..) 
  
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

-- Hmm, maybe BoundingBox should always have rectangular coords
-- i.e. Point2 and it should be parameterized on the numeric unit 
-- instead (like it used to be).

-- | Bounding box, two point representation (bottom-left and top-right). 
data BoundingBox pt = BBox { bbBottomLeft :: pt, bbTopRight :: pt }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox (Point2 Double)



instance HasPoints DBoundingBox  where
  type Pnt DBoundingBox = Point2 Double
  extractPoints (BBox p1@(P2 xmin ymin) p2@(P2 xmax ymax)) = [p1,br,p2,tl]
    where br = P2 xmax ymin
          tl = P2 xmin ymax
  endPoint (BBox start _) = start       -- start point is also end point   
  startPoint              = endPoint  

instance (Fractional a, Ord a) => Monoid (BoundingBox (Point2 a)) where
  mempty  = BBox (P2 inf inf) (P2 (-inf) (-inf)) where inf = 1/0
  mappend = bbUnion

-------------------------------------------------------------------------------


class HasBoundingBox sh pt where
  getBoundingBox :: sh -> BoundingBox pt

instance Pointwise (BoundingBox pt) where
  type Pt (BoundingBox pt) = pt
  pointwise fn (BBox a b) = BBox (fn a) (fn b)

                
--------------------------------------------------------------------------------
-- Construction


-- | Calculate the bounding box of a polygon.
instance Ord a => HasBoundingBox (Polygon (Point2 a)) (Point2 a) where
  getBoundingBox (Polygon ps) = bounds' ps



bounds :: (HasPoints t, Ord a, Pnt t ~ Point2 a) 
       => t -> BoundingBox (Point2 a)
bounds = bounds' . extractPoints

bounds' :: Ord a => [Point2 a] -> BoundingBox (Point2 a)
bounds' []     = error $ "Polygon.bounds' - empty list"
bounds' (p:ps) = uncurry BBox $ foldr fn (p,p) ps
  where
    fn (P2 x y) (P2 xmin ymin, P2 xmax ymax) = 
       (P2 (min x xmin) (min y ymin), P2 (max x xmax) (max y ymax))


--------------------------------------------------------------------------------
-- Operations



bbUnion :: Ord a 
       => BoundingBox (Point2 a) 
       -> BoundingBox (Point2 a) 
       -> BoundingBox (Point2 a)
bbUnion (BBox (P2 xmin1 ymin1) (P2 xmax1 ymax1))
        (BBox (P2 xmin2 ymin2) (P2 xmax2 ymax2))
  = BBox (P2 (min xmin1 xmin2) (min ymin1 ymin2)) 
         (P2 (max xmax1 xmax2) (max ymax1 ymax2))



within :: Ord a => Point2 a -> BoundingBox (Point2 a) -> Bool
within (P2 x y) (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
   x >= xmin && x <= xmax && y >= ymin && y <= ymax


width :: Num a => BoundingBox (Point2 a) -> a
width (BBox (P2 xmin _) (P2 xmax _)) = xmin + (xmax-xmin)


height :: Num a => BoundingBox (Point2 a) -> a
height (BBox (P2 _ ymin) (P2 _ ymax)) = ymin + (ymax-ymin)


-- | Extract the opposite corners (tl,br) of a bounding box.
oppositeCorners :: BoundingBox (Point2 a) -> (Point2 a,Point2 a)
oppositeCorners (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
  (P2 xmin ymax, P2 xmax ymin) 

--------------------------------------------------------------------------------
-- Points on a bounding box

class ReferencePoints pt where
  -- | Center (middle point) of a bounding box.
  center    :: BoundingBox pt -> pt
  -- | @north@ - middle point on the top of a bounding box.
  north     :: BoundingBox pt -> pt
  -- | @south@ - middle point on the bottom of a bounding box.
  south     :: BoundingBox pt -> pt
  -- | @east@ - middle point on the right side of the bounding box.
  east      :: BoundingBox pt -> pt
  -- | @west@ - middle point on the left side of the bounding box.
  west      :: BoundingBox pt -> pt
  -- | @northEast@ top-right corner of the bounding box.
  northEast :: BoundingBox pt -> pt
  -- | @southEast@ - bottom-right corner of the bounding box.
  southEast :: BoundingBox pt -> pt
  -- | @southWest@ - bottom-left corner of the bounding box.
  southWest :: BoundingBox pt -> pt
  -- | @northWest@ - top-left corner of the bounding box.
  northWest :: BoundingBox pt -> pt 

  -- defaults
  northEast = bbTopRight
  southWest = bbBottomLeft



instance Fractional a => ReferencePoints (Point2 a) where
  center (BBox (P2 xmin ymin) (P2 xmax ymax)) = 
    P2 (xmin + (0.5*(xmax-xmin))) (ymin + (0.5*(ymax-ymin))) 

  north (BBox (P2 xmin _) (P2 xmax ymax)) = P2 (xmin + 0.5*(xmax-xmin)) ymax

  south (BBox (P2 xmin ymin) (P2 xmax _)) = P2 (xmin + 0.5*(xmax-xmin)) ymin

  east (BBox (P2 _ ymin) (P2 xmax ymax)) = P2 xmax (ymin + 0.5*(ymax-ymin))

  west (BBox (P2 xmin ymin) (P2 _ ymax)) = P2 xmin (ymin + 0.5*(ymax-ymin))

  southEast (BBox (P2 _ y) (P2 x _)) = P2 x y

  northWest (BBox (P2 x _) (P2 _ y))     = P2 x y
 


-- | Center a shape at the supplied point.

centeredAt :: (Fractional a, Ord a, HasPoints shape, Pointwise shape, 
               AffineSpace (Pt shape), 
               Pnt shape ~ Point2 a, Diff (Pt shape) ~ Vec2 a) 
           => shape -> Point2 a -> shape
centeredAt sh pt = pointwise (.+^ diff) sh
  where
    diff = pt .-. center (bounds sh) 





