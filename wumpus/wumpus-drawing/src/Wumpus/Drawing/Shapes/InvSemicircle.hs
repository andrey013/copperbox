{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.InvSemicircle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Inverse semicircle. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.InvSemicircle
  ( 

    InvSemicircle
  , DInvSemicircle
  , invsemicircle

  ) where

import Wumpus.Drawing.Shapes.Base
import Wumpus.Drawing.Shapes.Semicircle

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core




--------------------------------------------------------------------------------
-- Inverse semicircle

newtype InvSemicircle u = InvSemicircle { getInvSemicircle :: Semicircle u }

type instance DUnit (InvSemicircle u) = u
  
type DInvSemicircle = InvSemicircle Double


instance Functor InvSemicircle where
  fmap f = InvSemicircle . fmap f . getInvSemicircle

--------------------------------------------------------------------------------
-- Affine trans

mapInner :: (Semicircle u -> Semicircle u) 
         -> InvSemicircle u 
         -> InvSemicircle u
mapInner f = InvSemicircle . f . getInvSemicircle


instance (Real u, Floating u) => Rotate (InvSemicircle u) where
  rotate ang            = mapInner (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (InvSemicircle u) where
  rotateAbout ang pt    = mapInner (rotateAbout ang pt)

instance Fractional u => Scale (InvSemicircle u) where
  scale sx sy           = mapInner (scale sx sy)

instance InterpretUnit u => Translate (InvSemicircle u) where
  translate dx dy       = mapInner (translate dx dy)



--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u) 
                => (Semicircle u -> Anchor u) -> InvSemicircle u -> Anchor u
runRotateAnchor f (InvSemicircle a) = 
    let ctr = center a in rotateAbout pi ctr (f a)


instance (Real u, Floating u) => 
    CenterAnchor InvSemicircle u where
  center = center . getInvSemicircle

instance (Real u, Floating u) => 
    ApexAnchor InvSemicircle u where
  apex = runRotateAnchor apex

instance (Real u, Floating u) => 
    TopCornerAnchor InvSemicircle u where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner

instance (Real u, Floating u) => 
    CardinalAnchor InvSemicircle u where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, LengthTolerance u) => 
    CardinalAnchor2 InvSemicircle u where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, LengthTolerance u) => 
    RadialAnchor InvSemicircle u where
  radialAnchor theta = 
    runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)


--------------------------------------------------------------------------------
-- Construction

-- | 'invsemicircle'  : @ radius -> Shape @
--
invsemicircle :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
           => u -> Shape InvSemicircle u
invsemicircle radius = 
    shapeMap InvSemicircle $ updatePathAngle (+ pi) $ semicircle radius
