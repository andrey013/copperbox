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
  , destBoundingBox
  , boundaryUnion 
  , traceBoundary
  , retraceBoundary

  , boundaryCorners
  , withinBoundary
  , boundaryWidth
  , boundaryHeight



  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.FormatCombinators
import Wumpus.Core.Geometry
import Wumpus.Core.Utils ( PSUnit(..) )

import Data.Semigroup                               -- package: algebra



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
  append = boundaryUnion


instance PSUnit u => Format (BoundingBox u) where
  format (BBox p0 p1) = parens (text "BBox" <+> text "ll=" <> format p0 
                                            <+> text "ur=" <> format p1) 


--------------------------------------------------------------------------------
-- 

type instance DUnit (BoundingBox u) = u

pointTransform :: (Num u , Ord u)
               => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
pointTransform fn bb = traceBoundary $ map fn $ [bl,br,tr,tl]
    where 
      (bl,br,tr,tl) = boundaryCorners bb


instance (Num u, Ord u) => Transform (BoundingBox u) where
  transform mtrx = pointTransform  (mtrx *#)

instance (Real u, Floating u) => Rotate (BoundingBox u) where
  rotate theta = pointTransform (rotate theta)

instance (Real u, Floating u) => RotateAbout (BoundingBox u) where
  rotateAbout theta pt = pointTransform (rotateAbout theta pt)

instance (Num u, Ord u) => Scale (BoundingBox u) where
  scale sx sy = pointTransform (scale sx sy)

instance (Num u, Ord u) => Translate (BoundingBox u) where
  translate dx dy = pointTransform (translate dx dy)


--------------------------------------------------------------------------------
-- Boundary class

-- | Type class extracting the bounding box of an object - 
-- Picture, Path etc.
--
class Boundary t where
  boundary :: u ~ DUnit t => t -> BoundingBox u 


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

-- | Destructor for BoundingBox.
--
-- Assembles a four-tuple @ (ll_x, ll_y, ur_x, ur_y) @.
-- 
-- Arguably this is easier to pattern match upon as it removes a 
-- layer of nesting.
--
destBoundingBox :: BoundingBox u -> (u,u,u,u)
destBoundingBox (BBox (P2 llx lly) (P2 urx ury)) = (llx, lly, urx, ury) 


-- | The union of two bounding boxes. This is also the @append@ 
-- of BoundingBox\'s @Semigroup@ instance.
--
boundaryUnion :: Ord u => BoundingBox u -> BoundingBox u -> BoundingBox u
BBox ll ur `boundaryUnion` BBox ll' ur' = BBox (minPt ll ll') (maxPt ur ur')

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
retraceBoundary f = traceBoundary . map f . fromCorners . boundaryCorners
  where
    fromCorners (bl,br,tr,tl) = [bl,br,tr,tl]


-- | Generate all the corners of a bounding box, counter-clock 
-- wise from the bottom left, i.e. @(bl, br, tr, tl)@.
boundaryCorners :: BoundingBox u -> (Point2 u, Point2 u, Point2 u, Point2 u)
boundaryCorners (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = (bl, br, tr, tl) where
    br = P2 x1 y0
    tl = P2 x0 y1

-- | Within test - is the supplied point within the bounding box?
--
withinBoundary :: Ord u => Point2 u -> BoundingBox u -> Bool
withinBoundary p (BBox ll ur) = (minPt p ll) == ll && (maxPt p ur) == ur

-- | Extract the width of a bounding box.
--
boundaryWidth :: Num u => BoundingBox u -> u
boundaryWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin

-- | Extract the height of a bounding box.
--
boundaryHeight :: Num u => BoundingBox u -> u
boundaryHeight (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin




