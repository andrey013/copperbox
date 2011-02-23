{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.InvTriangle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Inverse version of the Triangle shape.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.InvTriangle
  ( 

    InvTriangle
  , DInvTriangle
  , invtriangle


  ) where

import Wumpus.Drawing.Shapes.Base
import Wumpus.Drawing.Shapes.Triangle

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core






-- Datatype

newtype InvTriangle u = InvTriangle { getInvTriangle :: Triangle u }



type DInvTriangle = InvTriangle Double

type instance DUnit (InvTriangle u) = u


--------------------------------------------------------------------------------
-- Affine trans

mapTriangle :: (Triangle u -> Triangle u) -> InvTriangle u -> InvTriangle u
mapTriangle f = InvTriangle . f . getInvTriangle 


instance Num u => Scale (InvTriangle u) where
  scale sx sy = mapTriangle (scale sx sy)


instance Rotate (InvTriangle u) where
  rotate ang = mapTriangle (rotate ang)
                  

instance (Real u, Floating u, PtSize u) => RotateAbout (InvTriangle u) where
  rotateAbout ang pt = mapTriangle (rotateAbout ang pt)


instance PtSize u => Translate (InvTriangle u) where
  translate dx dy = mapTriangle (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- Anchors should be rotated about the center by pi...

runRotateAnchor :: (Real u, Floating u, PtSize u) 
                => (Triangle u -> Point2 u) -> InvTriangle u -> Point2 u
runRotateAnchor f (InvTriangle a) = rotateAbout pi (center a) (f a)


instance (Real u, Floating u, PtSize u) => CenterAnchor (InvTriangle u) where
  center = center . getInvTriangle


-- apex is same on InvTriangle as regular triangle

instance (Real u, Floating u, PtSize u) => ApexAnchor (InvTriangle u) where
  apex = runRotateAnchor apex

-- Top corners are bottom corners of the wrapped triangle.
--
instance (Real u, Floating u, PtSize u) => TopCornerAnchor (InvTriangle u) where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner


-- Use established points on the InvTrangle - don\'t delegate to 
-- the base Triangle.
--
instance (Real u, Floating u, PtSize u) => SideMidpointAnchor (InvTriangle u) where
  sideMidpoint n a = step (n `mod` 3) 
    where
      step 1 = midpoint (topRightCorner a) (topLeftCorner a)
      step 2 = midpoint (topLeftCorner a)  (apex a)
      step _ = midpoint (apex a)           (topRightCorner a)



-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u, PtSize u) => 
    CardinalAnchor (InvTriangle u) where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, PtSize u) => 
    CardinalAnchor2 (InvTriangle u) where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, PtSize u) => RadialAnchor (InvTriangle u) where
  radialAnchor theta = runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)

--------------------------------------------------------------------------------
-- Construction

-- | 'invtriangle'  : @ top_base_width * height -> Triangle @
--
--
invtriangle :: (Real u, Floating u, PtSize u)
            => u -> u -> Shape u (InvTriangle u)
invtriangle bw h = fmap InvTriangle $ updatePathAngle (+ pi) $ triangle bw h
    



