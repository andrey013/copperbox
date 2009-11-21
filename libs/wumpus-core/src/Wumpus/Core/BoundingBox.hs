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
-- Bounding box with no notion of \'empty\'.
--
--------------------------------------------------------------------------------

module Wumpus.Core.BoundingBox 
  ( 
  -- * Types
    BoundingBox(..)
  , DBoundingBox
  , CardinalPoint(..)

  -- * Type class
  , Boundary(..)
  
  -- * Operations
  , bbox
  , obbox
  , union 
  , trace
  , corners
  , lowerLeftUpperRight
  , withinBB
  , boundaryWidth
  , boundaryHeight
  , boundaryBottomLeft
  , boundaryTopRight
  , boundaryTopLeft
  , boundaryBottomRight
  , boundaryPoint
  , leftPlane
  , rightPlane
  , lowerPlane
  , upperPlane

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.Utils ( CMinMax(..), within )

import Data.Semigroup

import Text.PrettyPrint.Leijen hiding ( width )



-- | Bounding box of a picture.
-- 
-- We cannot construct empty pictures - so bounding boxes too a 
-- saved the obligation to be empty.
-- 
data BoundingBox a = BBox { 
                         ll_corner :: Point2 a, 
                         ur_corner :: Point2 a 
                       }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double

data CardinalPoint = C | N | NE | E | SE | S | SW | W | NW
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- instances

-- BBox is NOT monoidal - it\'s much simpler that way.

instance Ord a => Semigroup (BoundingBox a) where
  append = union


instance Pretty a => Pretty (BoundingBox a) where
  pretty (BBox p0 p1) = text "|_" <+> pretty p0 <+> pretty p1 <+> text "_|" 


--------------------------------------------------------------------------------
-- 

type instance DUnit (BoundingBox u) = u

instance (Num u, Ord u) => Scale (BoundingBox u) where
  scale x y bb     = trace $ map (scale x y) $ corners bb



--------------------------------------------------------------------------------
-- Boundary class

class Boundary a where
  boundary :: a -> BoundingBox (DUnit a)


--------------------------------------------------------------------------------


instance Pointwise (BoundingBox a) where
  type Pt (BoundingBox a) = Point2 a
  pointwise f (BBox bl tr) = BBox (f bl) (f tr)


--------------------------------------------------------------------------------

bbox :: Point2 a -> Point2 a -> BoundingBox a
bbox = BBox 


-- | Create a BoundingBox with bottom left corner at the origin,
-- and dimensions @w@ and @h@.
obbox :: Num a => a -> a -> BoundingBox a
obbox w h = BBox zeroPt (P2 w h)


union :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
BBox ll ur `union` BBox ll' ur' = BBox (cmin ll ll') (cmax ur ur')

-- Trace the point list finding the /extremity/...

trace :: (Num a, Ord a) => [Point2 a] -> BoundingBox a
trace (p:ps) = uncurry BBox $ foldr (\z (a,b) -> (cmin z a, cmax z b) ) (p,p) ps
trace []     = error $ "BoundingBox.trace called in empty list"


corners :: BoundingBox a -> [Point2 a]
corners (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = [bl, br, tr, tl] where
    br = P2 x1 y0
    tl = P2 x0 y1


lowerLeftUpperRight :: (a,a,a,a) -> BoundingBox a -> (a,a,a,a)
lowerLeftUpperRight _   (BBox (P2 x0 y0) (P2 x1 y1)) = (x0,y0,x1,y1)



withinBB :: Ord a => Point2 a -> BoundingBox a -> Bool
withinBB p (BBox ll ur) = within p ll ur


boundaryWidth :: Num a => BoundingBox a -> a
boundaryWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

boundaryHeight :: Num a => BoundingBox a -> a
boundaryHeight (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin


--------------------------------------------------------------------------------

-- Points on the boundary


boundaryBottomLeft  :: BoundingBox a -> Point2 a
boundaryBottomLeft (BBox p0 _ ) = p0

boundaryTopRight :: BoundingBox a -> Point2 a
boundaryTopRight (BBox _ p1) = p1

boundaryTopLeft :: BoundingBox a -> Point2 a
boundaryTopLeft (BBox (P2 x _) (P2 _ y)) = P2 x y

boundaryBottomRight :: BoundingBox a -> Point2 a
boundaryBottomRight (BBox (P2 _ y) (P2 x _)) = P2 x y


boundaryPoint :: Fractional a 
              => CardinalPoint -> BoundingBox a -> Point2 a
boundaryPoint loc (BBox (P2 x0 y0) (P2 x1 y1)) = fn loc where
  fn C      = P2 xMid   yMid
  fn N      = P2 xMid   y1
  fn NE     = P2 x1     y1
  fn E      = P2 x1     yMid
  fn SE     = P2 x1     y0
  fn S      = P2 xMid   y0
  fn SW     = P2 x0     y0
  fn W      = P2 x0     yMid
  fn NW     = P2 x0     y1       

  xMid      = x0 + 0.5 * (x1 - x0)
  yMid      = y0 + 0.5 * (y1 - y0)


--------------------------------------------------------------------------------

-- /planes/ on the bounding box

-- Are these really worthwhile ? ...

leftPlane :: BoundingBox a -> a
leftPlane (BBox (P2 l _) _) = l

rightPlane :: BoundingBox a -> a
rightPlane (BBox _ (P2 r _)) = r

lowerPlane :: BoundingBox a -> a
lowerPlane (BBox (P2 _ l) _) = l

upperPlane :: BoundingBox a -> a
upperPlane (BBox _ (P2 _ u)) = u






