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

import Wumpus.Drawing.Geometry.Quadrant
import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base
import Wumpus.Drawing.Shapes.Triangle

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




--------------------------------------------------------------------------------
-- InvTriangle

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

appTriangle :: (Triangle u -> a) -> InvTriangle u -> a
appTriangle f = f . getInvTriangle


instance (Real u, Floating u) => CenterAnchor (InvTriangle u) where
  center = appTriangle center 




-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u) => CardinalAnchor (InvTriangle u) where
  north = appTriangle south
  south = appTriangle north 
  east  = appTriangle west 
  west  = appTriangle east


instance (Real u, Floating u) => CardinalAnchor2 (InvTriangle u) where
  northeast = appTriangle southwest
  southeast = appTriangle northwest
  southwest = appTriangle northeast
  northwest = appTriangle southeast



instance (Real u, Floating u) => RadialAnchor (InvTriangle u) where
  radialAnchor theta = appTriangle (radialAnchor ang) 
    where
      ang = circularModulo $ theta + pi

--------------------------------------------------------------------------------
-- Constructor 

-- | 'invtriangle'  : @ top_base_width * height -> Triangle @
--
--
invtriangle :: (Real u, Floating u, FromPtSize u)
            => u -> u -> LocShape u (InvTriangle u)
invtriangle bw h = mapAns InvTriangle $ rtriangle bw h pi
    

mapAns :: Functor f => (a -> z) -> f (a,b) -> f (z,b)
mapAns f = fmap (\(a,b) -> (f a ,b))

