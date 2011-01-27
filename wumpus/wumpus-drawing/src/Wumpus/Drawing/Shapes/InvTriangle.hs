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

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

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
                  

instance (Real u, Floating u) => RotateAbout (InvTriangle u) where
  rotateAbout ang pt = mapTriangle (rotateAbout ang pt)


instance Num u => Translate (InvTriangle u) where
  translate dx dy = mapTriangle (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- I think anchors should be rotated about the center by pi...

runRotateAnchor :: (Real u, Floating u) 
                => (Triangle u -> Point2 u) -> InvTriangle u -> Point2 u
runRotateAnchor f (InvTriangle a) = rotateAbout pi (center a) (f a)


instance (Real u, Floating u) => CenterAnchor (InvTriangle u) where
  center = center . getInvTriangle




-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u) => CardinalAnchor (InvTriangle u) where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u) => CardinalAnchor2 (InvTriangle u) where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u) => RadialAnchor (InvTriangle u) where
  radialAnchor theta = runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)

--------------------------------------------------------------------------------
-- Construction

-- | 'invtriangle'  : @ top_base_width * height -> Triangle @
--
--
invtriangle :: (Real u, Floating u, FromPtSize u)
            => u -> u -> Shape u (InvTriangle u)
invtriangle bw h = fmap InvTriangle $ updatePathAngle (+ pi) $ triangle bw h
    



