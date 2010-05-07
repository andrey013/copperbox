{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.BoundingBox
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Bounding box with no notion of \'empty\'.
--
-- Empty pictures cannot be created with Wumpus. This greatly 
-- simplifies the implementation of pictures themselves and
-- bounding boxes.
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



-- | Bounding box of a picture, represented by the lower left and
-- upper right corners.
-- 
-- We cannot construct empty pictures - so bounding boxes are 
-- spared the obligation to be /empty/. BoundingBox is an instance
-- of the Semigroup class where @append@ is the union operation.
-- 
data BoundingBox a = BBox { 
                         ll_corner :: Point2 a, 
                         ur_corner :: Point2 a 
                       }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double

-- | A location on a bounding box - the center @C@ and the usual
-- compass points @N@, @S@, etc.
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

-- | Type class extracting the bounding box of an object - 
-- Picture, Path etc.
class Boundary a where
  boundary :: a -> BoundingBox (DUnit a)


--------------------------------------------------------------------------------


instance Pointwise (BoundingBox a) where
  type Pt (BoundingBox a) = Point2 a
  pointwise f (BBox bl tr) = BBox (f bl) (f tr)


--------------------------------------------------------------------------------

-- | Contruct a bounding box, vis the BBox constructor with range 
-- checking on the corner points.
--
-- @bbox@ throws an error if the width or height of the 
-- constructed bounding box is negative.
--
bbox :: Ord a => Point2 a -> Point2 a -> BoundingBox a
bbox ll@(P2 x0 y0) ur@(P2 x1 y1) 
   | x0 <= x1 && y0 <= y1 = BBox ll ur 
   | otherwise            = error "Wumpus.Core.BoundingBox.bbox - malformed."


-- | Create a BoundingBox with bottom left corner at the origin,
-- and dimensions @w@ and @h@.
obbox :: Num a => a -> a -> BoundingBox a
obbox w h = BBox zeroPt (P2 w h)


-- | The union of two bounding boxes. This is also the @append@ 
-- of BoundingBox\'s @Semigroup@ instance.
union :: Ord a => BoundingBox a -> BoundingBox a -> BoundingBox a
BBox ll ur `union` BBox ll' ur' = BBox (cmin ll ll') (cmax ur ur')

-- | Trace a list of points, retuning the BoundingBox that 
-- includes them.
trace :: (Num a, Ord a) => [Point2 a] -> BoundingBox a
trace (p:ps) = uncurry BBox $ foldr (\z (a,b) -> (cmin z a, cmax z b) ) (p,p) ps
trace []     = error $ "BoundingBox.trace called in empty list"

-- | Generate all the corners of a bounding box, counter-clock 
-- wise from the bottom left, i.e. @[bl, br, tr, tl]@.
corners :: BoundingBox a -> [Point2 a]
corners (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = [bl, br, tr, tl] where
    br = P2 x1 y0
    tl = P2 x0 y1

-- | Witinh test - is the supplied point within the bounding box?
withinBB :: Ord a => Point2 a -> BoundingBox a -> Bool
withinBB p (BBox ll ur) = within p ll ur

-- | Extract the width of a bounding box.
boundaryWidth :: Num a => BoundingBox a -> a
boundaryWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

-- | Extract the height of a bounding box.
boundaryHeight :: Num a => BoundingBox a -> a
boundaryHeight (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin


--------------------------------------------------------------------------------

-- Points on the boundary

-- | Extract the bottom-left corner of the bounding box.
boundaryBottomLeft  :: BoundingBox a -> Point2 a
boundaryBottomLeft (BBox p0 _ ) = p0

-- | Extract the top-right corner of the bounding box.
boundaryTopRight :: BoundingBox a -> Point2 a
boundaryTopRight (BBox _ p1) = p1

-- | Extract the top-left corner of the bounding box.
boundaryTopLeft :: BoundingBox a -> Point2 a
boundaryTopLeft (BBox (P2 x _) (P2 _ y)) = P2 x y

-- | Extract the bottom-right corner of the bounding box.
boundaryBottomRight :: BoundingBox a -> Point2 a
boundaryBottomRight (BBox (P2 _ y) (P2 x _)) = P2 x y


-- | Extract a point from the bounding box at the supplied 
-- cardinal position.
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

-- | Extract the unit of the left vertical plane.
leftPlane :: BoundingBox a -> a
leftPlane (BBox (P2 l _) _) = l

-- | Extract the unit of the right vertical plane.
rightPlane :: BoundingBox a -> a
rightPlane (BBox _ (P2 r _)) = r

-- | Extract the unit of the lower horizontal plane.
lowerPlane :: BoundingBox a -> a
lowerPlane (BBox (P2 _ l) _) = l

-- | Extract the unit of the upper horizontal plane.
upperPlane :: BoundingBox a -> a
upperPlane (BBox _ (P2 _ u)) = u






