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

  
type DInvSemiellipse = InvSemiellipse Double



instance Functor InvSemiellipse where
  fmap f = InvSemiellipse . fmap f . getInvSemiellipse

--------------------------------------------------------------------------------
-- Affine trans

mapSemiellipse :: (Semiellipse u -> Semiellipse u) 
              -> InvSemiellipse u 
              -> InvSemiellipse u
mapSemiellipse f = InvSemiellipse . f . getInvSemiellipse

instance Num u => Scale (InvSemiellipse u) where
  scale sx sy = mapSemiellipse (scale sx sy)


instance Rotate (InvSemiellipse u) where
  rotate ang = mapSemiellipse (rotate ang)
                  

instance (Real u, Floating u, InterpretUnit u) => RotateAbout (InvSemiellipse u) where
  rotateAbout ang pt = mapSemiellipse (rotateAbout ang pt)


instance InterpretUnit u => Translate (InvSemiellipse u) where
  translate dx dy = mapSemiellipse (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u, InterpretUnit u) 
                => (Semiellipse u -> Anchor u) -> InvSemiellipse u -> Anchor u
runRotateAnchor f (InvSemiellipse a) =
   center a >>= \ctr -> f a >>= \a1 -> rotateAboutCtx pi ctr a1



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CenterAnchor InvSemiellipse u where
  center = center . getInvSemiellipse

instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    ApexAnchor InvSemiellipse u where
  apex = runRotateAnchor apex

instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    TopCornerAnchor InvSemiellipse u where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner

instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor InvSemiellipse u where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor2 InvSemiellipse u where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    RadialAnchor InvSemiellipse u where
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
