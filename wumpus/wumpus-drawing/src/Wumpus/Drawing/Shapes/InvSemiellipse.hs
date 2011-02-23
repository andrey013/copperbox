{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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

type instance DUnit (InvSemiellipse u) = u


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
                  

instance (Real u, Floating u, PtSize u) => RotateAbout (InvSemiellipse u) where
  rotateAbout ang pt = mapSemiellipse (rotateAbout ang pt)


instance PtSize u => Translate (InvSemiellipse u) where
  translate dx dy = mapSemiellipse (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u, PtSize u) 
                => (Semiellipse u -> Point2 u) -> InvSemiellipse u -> Point2 u
runRotateAnchor f (InvSemiellipse a) = rotateAbout pi (center a) (f a)


instance (Real u, Floating u, PtSize u) => CenterAnchor (InvSemiellipse u) where
  center = center . getInvSemiellipse

instance (Real u, Floating u, PtSize u) => 
    ApexAnchor (InvSemiellipse u) where
  apex = runRotateAnchor apex

instance (Real u, Floating u, PtSize u) => 
    TopCornerAnchor (InvSemiellipse u) where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner

instance (Real u, Floating u, PtSize u) => 
    CardinalAnchor (InvSemiellipse u) where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, PtSize u) => 
    CardinalAnchor2 (InvSemiellipse u) where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, PtSize u) => 
    RadialAnchor (InvSemiellipse u) where
  radialAnchor theta = 
    runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)


--------------------------------------------------------------------------------
-- Construction

-- | 'invsemiellipse'  : @ rx * ry -> Shape @
--
invsemiellipse :: (Real u, Floating u, PtSize u) 
           => u -> u -> Shape u (InvSemiellipse u)
invsemiellipse rx ry = 
    fmap InvSemiellipse $ updatePathAngle (+ pi) $ semiellipse rx ry
