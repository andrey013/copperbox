{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.InvSemiellipse
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Inverse semiellipse. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.InvSemiellipse
  ( 

    InvSemiellipse
  , DInvSemiellipse
  , invsemiellipse

  ) where

import Wumpus.Drawing.Shapes.Base
import Wumpus.Drawing.Shapes.Semiellipse

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core




--------------------------------------------------------------------------------
-- Inverse semiellipse

newtype InvSemiellipse u = InvSemiellipse { getInvSemiellipse :: Semiellipse u }

type instance DUnit (InvSemiellipse u) = u
  
type DInvSemiellipse = InvSemiellipse Double



instance Functor InvSemiellipse where
  fmap f = InvSemiellipse . fmap f . getInvSemiellipse

--------------------------------------------------------------------------------
-- Affine trans

mapInner :: (Semiellipse u -> Semiellipse u) 
         -> InvSemiellipse u 
         -> InvSemiellipse u
mapInner f = InvSemiellipse . f . getInvSemiellipse

instance (Real u, Floating u) => Rotate (InvSemiellipse u) where
  rotate ang            = mapInner (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (InvSemiellipse u) where
  rotateAbout ang pt    = mapInner (rotateAbout ang pt)

instance Fractional u => Scale (InvSemiellipse u) where
  scale sx sy           = mapInner (scale sx sy)

instance InterpretUnit u => Translate (InvSemiellipse u) where
  translate dx dy       = mapInner (translate dx dy)



--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u) 
                => (Semiellipse u -> Anchor u) -> InvSemiellipse u -> Anchor u
runRotateAnchor f (InvSemiellipse a) =
    let ctr = center a in rotateAbout pi ctr (f a)



instance (Real u, Floating u, LengthTolerance u) => 
    CenterAnchor (InvSemiellipse u) where
  center = center . getInvSemiellipse

instance (Real u, Floating u, LengthTolerance u) => 
    ApexAnchor (InvSemiellipse u) where
  apex = runRotateAnchor apex

instance (Real u, Floating u, LengthTolerance u) => 
    TopCornerAnchor (InvSemiellipse u) where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner

instance (Real u, Floating u, LengthTolerance u) => 
    CardinalAnchor (InvSemiellipse u) where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, LengthTolerance u) => 
    CardinalAnchor2 (InvSemiellipse u) where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, LengthTolerance u) => 
    RadialAnchor (InvSemiellipse u) where
  radialAnchor theta = 
    runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)


--------------------------------------------------------------------------------
-- Construction

-- | 'invsemiellipse'  : @ rx * ry -> Shape @
--
invsemiellipse :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
           => u -> u -> Shape InvSemiellipse u
invsemiellipse rx ry = 
    shapeMap InvSemiellipse $ updatePathAngle (+ pi) $ semiellipse rx ry
