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


--------------------------------------------------------------------------------
-- Affine trans

mapSemicircle :: (Semicircle u -> Semicircle u) 
              -> InvSemicircle u 
              -> InvSemicircle u
mapSemicircle f = InvSemicircle . f . getInvSemicircle

instance Num u => Scale (InvSemicircle u) where
  scale sx sy = mapSemicircle (scale sx sy)


instance Rotate (InvSemicircle u) where
  rotate ang = mapSemicircle (rotate ang)
                  

instance (Real u, Floating u, PtSize u) => RotateAbout (InvSemicircle u) where
  rotateAbout ang pt = mapSemicircle (rotateAbout ang pt)


instance PtSize u => Translate (InvSemicircle u) where
  translate dx dy = mapSemicircle (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u, PtSize u) 
                => (Semicircle u -> Point2 u) -> InvSemicircle u -> Point2 u
runRotateAnchor f (InvSemicircle a) = rotateAbout pi (center a) (f a)


instance (Real u, Floating u, PtSize u) => 
    CenterAnchor (InvSemicircle u) u where
  center = center . getInvSemicircle

instance (Real u, Floating u, PtSize u) => 
    ApexAnchor (InvSemicircle u) u where
  apex = runRotateAnchor apex

instance (Real u, Floating u, PtSize u) => 
    TopCornerAnchor (InvSemicircle u) u where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner

instance (Real u, Floating u, PtSize u) => 
    CardinalAnchor (InvSemicircle u) u where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, PtSize u) => 
    CardinalAnchor2 (InvSemicircle u) u where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, PtSize u) => 
    RadialAnchor (InvSemicircle u) u where
  radialAnchor theta = 
    runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)


--------------------------------------------------------------------------------
-- Construction

-- | 'invsemicircle'  : @ radius -> Shape @
--
invsemicircle :: (Real u, Floating u, PtSize u) 
           => u -> Shape InvSemicircle u
invsemicircle radius = 
    shapeMap InvSemicircle $ updatePathAngle (+ pi) $ semicircle radius
