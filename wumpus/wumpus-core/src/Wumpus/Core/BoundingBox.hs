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
-- Portability :  GHC
--
-- Bounding box with no notion of \'empty\'.
--
-- Empty pictures cannot be created with Wumpus. This greatly 
-- simplifies the implementation of pictures themselves and
-- bounding boxes.
-- 
-- WARNING - this module is no so tightly designed, with some 
-- of the functions seeming superfluous in hindsight. It is
-- likely to change in the future.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.BoundingBox 
  ( 
  -- * Types
    BoundingBox(..)
  , DBoundingBox

  -- * Type class
  , Boundary(..)
  
  -- * Operations
  , bbox
  , obbox
  , union 
  , traceBoundary
  , retraceBoundary

  , corners
  , within
  , boundaryWidth
  , boundaryHeight
  , boundaryBottomLeft
  , boundaryTopRight
  , boundaryTopLeft
  , boundaryBottomRight

  , leftPlane
  , rightPlane
  , lowerPlane
  , upperPlane

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry

import Data.Semigroup

import Text.PrettyPrint.Leijen hiding ( width )



-- | Bounding box of a picture, represented by the lower left and
-- upper right corners.
-- 
-- We cannot construct empty pictures - so bounding boxes are 
-- spared the obligation to be /empty/. BoundingBox is an instance
-- of the Semigroup class where @append@ is the union operation.
-- 
data BoundingBox u = BBox 
      { ll_corner :: Point2 u
      , ur_corner :: Point2 u 
      }
  deriving (Eq,Show)

type DBoundingBox = BoundingBox Double



--------------------------------------------------------------------------------
-- instances

-- BBox is NOT monoidal - it\'s much simpler that way.

instance Ord u => Semigroup (BoundingBox u) where
  append = union


instance Pretty u => Pretty (BoundingBox u) where
  pretty (BBox p0 p1) = text "|_" <+> pretty p0 <+> pretty p1 <+> text "_|" 


--------------------------------------------------------------------------------
-- 

type instance DUnit (BoundingBox u) = u

instance (Num u, Ord u) => Scale (BoundingBox u) where
  scale x y bb     = traceBoundary $ map (scale x y) $ [bl,br,tr,tl]
    where (bl,br,tr,tl) = corners bb



--------------------------------------------------------------------------------
-- Boundary class

-- | Type class extracting the bounding box of an object - 
-- Picture, Path etc.
--
class Boundary a where
  boundary :: DUnit a ~ u => a -> BoundingBox u 


--------------------------------------------------------------------------------


instance Pointwise (BoundingBox u) where
  type Pt (BoundingBox u) = Point2 u
  pointwise f (BBox bl tr) = BBox (f bl) (f tr)


--------------------------------------------------------------------------------

-- | Contruct a bounding box, vis the BBox constructor with range 
-- checking on the corner points.
--
-- @bbox@ throws an error if the width or height of the 
-- constructed bounding box is negative.
--
bbox :: Ord u => Point2 u -> Point2 u -> BoundingBox u
bbox ll@(P2 x0 y0) ur@(P2 x1 y1) 
   | x0 <= x1 && y0 <= y1 = BBox ll ur 
   | otherwise            = error "Wumpus.Core.BoundingBox.bbox - malformed."


-- | Create a BoundingBox with bottom left corner at the origin,
-- and dimensions @w@ and @h@.
--
obbox :: Num u => u -> u -> BoundingBox u
obbox w h = BBox zeroPt (P2 w h)


-- | The union of two bounding boxes. This is also the @append@ 
-- of BoundingBox\'s @Semigroup@ instance.
--
union :: Ord u => BoundingBox u -> BoundingBox u -> BoundingBox u
BBox ll ur `union` BBox ll' ur' = BBox (minPt ll ll') (maxPt ur ur')

-- | Trace a list of points, retuning the BoundingBox that 
-- includes them.
--
-- 'trace' throws a run-time error when supplied with the empty 
-- list.
--
traceBoundary :: (Num u, Ord u) => [Point2 u] -> BoundingBox u
traceBoundary (p:ps) = 
    uncurry BBox $ foldr (\z (a,b) -> (minPt z a, maxPt z b) ) (p,p) ps
traceBoundary []     = error $ "BoundingBox.trace called in empty list"

-- | Perform the supplied transformation on the four corners of 
-- the bounding box. Trace the new corners to calculate the 
-- resulting bounding box.
-- 
-- This helper function can be used to re-calculate a bounding 
-- box after a rotation for example.
--
retraceBoundary :: (Num u, Ord u) 
        => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
retraceBoundary f = traceBoundary . map f . fromCorners . corners
  where
    fromCorners (bl,br,tr,tl) = [bl,br,tr,tl]


-- | Generate all the corners of a bounding box, counter-clock 
-- wise from the bottom left, i.e. @(bl, br, tr, tl)@.
corners :: BoundingBox u -> (Point2 u, Point2 u, Point2 u, Point2 u)
corners (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = (bl, br, tr, tl) where
    br = P2 x1 y0
    tl = P2 x0 y1

-- | Within test - is the supplied point within the bounding box?
--
within :: Ord u => Point2 u -> BoundingBox u -> Bool
within p (BBox ll ur) = (minPt p ll) == ll && (maxPt p ur) == ur

-- | Extract the width of a bounding box.
--
boundaryWidth :: Num u => BoundingBox u -> u
boundaryWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

-- | Extract the height of a bounding box.
--
boundaryHeight :: Num u => BoundingBox u -> u
boundaryHeight (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin


--------------------------------------------------------------------------------

-- Points on the boundary

-- | Extract the bottom-left corner of the bounding box.
boundaryBottomLeft  :: BoundingBox u -> Point2 u
boundaryBottomLeft (BBox p0 _ ) = p0

-- | Extract the top-right corner of the bounding box.
boundaryTopRight :: BoundingBox u -> Point2 u
boundaryTopRight (BBox _ p1) = p1

-- | Extract the top-left corner of the bounding box.
boundaryTopLeft :: BoundingBox u -> Point2 u
boundaryTopLeft (BBox (P2 x _) (P2 _ y)) = P2 x y

-- | Extract the bottom-right corner of the bounding box.
boundaryBottomRight :: BoundingBox u -> Point2 u
boundaryBottomRight (BBox (P2 _ y) (P2 x _)) = P2 x y




--------------------------------------------------------------------------------

-- /planes/ on the bounding box

-- Are these really worthwhile ? ...

-- | Extract the unit of the left vertical plane.
leftPlane :: BoundingBox u -> u
leftPlane (BBox (P2 l _) _) = l

-- | Extract the unit of the right vertical plane.
rightPlane :: BoundingBox u -> u
rightPlane (BBox _ (P2 r _)) = r

-- | Extract the unit of the lower horizontal plane.
lowerPlane :: BoundingBox u -> u
lowerPlane (BBox (P2 _ l) _) = l

-- | Extract the unit of the upper horizontal plane.
upperPlane :: BoundingBox u -> u
upperPlane (BBox _ (P2 _ u)) = u






