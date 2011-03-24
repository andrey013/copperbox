{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
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

import Control.Applicative




-- Datatype

newtype InvTriangle u = InvTriangle { getInvTriangle :: Triangle u }

type DInvTriangle = InvTriangle Double


instance Functor InvTriangle where
  fmap f = InvTriangle . fmap f . getInvTriangle


--------------------------------------------------------------------------------
-- Affine trans

mapInner :: (Triangle u -> Triangle u) -> InvTriangle u -> InvTriangle u
mapInner f = InvTriangle . f . getInvTriangle 

instance InterpretUnit u => CtxRotate InvTriangle u where
  ctxRotate sz ang = mapInner (ctxRotate sz ang)
              
instance InterpretUnit u => CtxRotateAbout InvTriangle u where
  ctxRotateAbout sz ang pt = mapInner (ctxRotateAbout sz ang pt)

instance InterpretUnit u => CtxScale InvTriangle u where
  ctxScale sz sx sy = mapInner (ctxScale sz sx sy)

instance InterpretUnit u => CtxTranslate InvTriangle u where
  ctxTranslate sz dx dy = mapInner (ctxTranslate sz dx dy)



--------------------------------------------------------------------------------
-- Anchors

-- Anchors should be rotated about the center by pi...

runRotateAnchor :: (Real u, Floating u, InterpretUnit u) 
                => (Triangle u -> Anchor u) -> InvTriangle u -> Anchor u
runRotateAnchor f (InvTriangle a) =
   center a  >>= \ctr -> 
   f a       >>= \a1 -> 
   pointSize >>= \sz -> 
   return $ ctxRotateAbout sz pi ctr a1


instance (Real u, Floating u, InterpretUnit u) => 
    CenterAnchor InvTriangle u where
  center = center . getInvTriangle


-- apex is same on InvTriangle as regular triangle

instance (Real u, Floating u, InterpretUnit u) => 
    ApexAnchor InvTriangle u where
  apex = runRotateAnchor apex

-- Top corners are bottom corners of the wrapped triangle.
--
instance (Real u, Floating u, InterpretUnit u) => 
    TopCornerAnchor InvTriangle u where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner


-- Use established points on the InvTrangle - don\'t delegate to 
-- the base Triangle.
--
instance (Real u, Floating u, InterpretUnit u) => 
    SideMidpointAnchor InvTriangle u where
  sideMidpoint n a = step (n `mod` 3) 
    where
      step 1 = midpoint <$> topRightCorner a  <*> topLeftCorner a
      step 2 = midpoint <$> topLeftCorner a   <*> apex a
      step _ = midpoint <$> apex a            <*> topRightCorner a



-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u, InterpretUnit u) => 
    CardinalAnchor InvTriangle u where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, InterpretUnit u) => 
    CardinalAnchor2 InvTriangle u where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, InterpretUnit u) => 
    RadialAnchor InvTriangle u where
  radialAnchor theta = runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)

--------------------------------------------------------------------------------
-- Construction

-- | 'invtriangle'  : @ top_base_width * height -> Triangle @
--
--
invtriangle :: (Real u, Floating u, InterpretUnit u, LengthTolerance u)
            => u -> u -> Shape InvTriangle u
invtriangle bw h = 
    shapeMap InvTriangle $ updatePathAngle (+ pi) $ triangle bw h
    



