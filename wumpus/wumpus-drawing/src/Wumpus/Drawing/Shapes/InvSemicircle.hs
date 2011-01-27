{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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

type instance DUnit (InvSemicircle u) = u


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
                  

instance (Real u, Floating u) => RotateAbout (InvSemicircle u) where
  rotateAbout ang pt = mapSemicircle (rotateAbout ang pt)


instance Num u => Translate (InvSemicircle u) where
  translate dx dy = mapSemicircle (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u) 
                => (Semicircle u -> Point2 u) -> InvSemicircle u -> Point2 u
runRotateAnchor f (InvSemicircle a) = rotateAbout pi (center a) (f a)


instance (Real u, Floating u) => CenterAnchor (InvSemicircle u) where
  center = center . getInvSemicircle

instance (Real u, Floating u) => CardinalAnchor (InvSemicircle u) where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, FromPtSize u) => 
    CardinalAnchor2 (InvSemicircle u) where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, FromPtSize u) => 
    RadialAnchor (InvSemicircle u) where
  radialAnchor theta = 
    runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)


--------------------------------------------------------------------------------
-- Construction

-- | 'invsemicircle'  : @ radius -> Shape @
--
invsemicircle :: (Real u, Floating u, FromPtSize u) 
           => u -> Shape u (InvSemicircle u)
invsemicircle radius = 
    fmap InvSemicircle $ updatePathAngle (+ pi) $ semicircle radius
