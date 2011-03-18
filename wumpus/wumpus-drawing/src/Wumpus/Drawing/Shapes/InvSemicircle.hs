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

  
type DInvSemicircle = InvSemicircle Double


instance Functor InvSemicircle where
  fmap f = InvSemicircle . fmap f . getInvSemicircle

--------------------------------------------------------------------------------
-- Affine trans

mapSemicircle :: (Semicircle u -> Semicircle u) 
              -> InvSemicircle u 
              -> InvSemicircle u
mapSemicircle f = InvSemicircle . f . getInvSemicircle

instance Scale (InvSemicircle u) where
  scale sx sy = mapSemicircle (scale sx sy)


instance Rotate (InvSemicircle u) where
  rotate ang = mapSemicircle (rotate ang)
                  

instance RotateAbout (InvSemicircle u) where
  rotateAbout ang pt = mapSemicircle (rotateAbout ang pt)


instance Translate (InvSemicircle u) where
  translate dx dy = mapSemicircle (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u, InterpretUnit u) 
                => (Semicircle u -> Anchor u) -> InvSemicircle u -> Anchor u
runRotateAnchor f (InvSemicircle a) = 
   center a >>= \ctr -> f a >>= \a1 -> rotateAboutCtx pi ctr a1


instance (Real u, Floating u, InterpretUnit u) => 
    CenterAnchor InvSemicircle u where
  center = center . getInvSemicircle

instance (Real u, Floating u, InterpretUnit u) => 
    ApexAnchor InvSemicircle u where
  apex = runRotateAnchor apex

instance (Real u, Floating u, InterpretUnit u) => 
    TopCornerAnchor InvSemicircle u where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner

instance (Real u, Floating u, InterpretUnit u) => 
    CardinalAnchor InvSemicircle u where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor2 InvSemicircle u where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
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
