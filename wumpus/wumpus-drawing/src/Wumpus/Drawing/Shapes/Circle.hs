{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Circle
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Simple shapes - rectangle, circle diamond, ellipse.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Circle
  ( 

    Circle
  , DCircle
  , circle

  ) where

import Wumpus.Drawing.Paths.Absolute
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



--------------------------------------------------------------------------------
-- Circle

data Circle u = Circle 
      { circ_ctm    :: ShapeCTM u
      , circ_radius :: !u 
      }

type instance DUnit (Circle u) = u  

type DCircle = Circle Double

instance Functor Circle where
  fmap f (Circle ctm r) = Circle (fmap f ctm) (f r)


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Circle u -> Circle u
mapCTM f = (\s i -> s { circ_ctm = f i }) <*> circ_ctm



instance (Real u, Floating u) => Rotate (Circle u) where
  rotate ang            = mapCTM (rotate ang)
                  
instance (Real u, Floating u) => RotateAbout (Circle u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Circle u) where
  scale sx sy           = mapCTM (scale sx sy)

instance Num u => Translate (Circle u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runDisplaceCenter :: (Real u, Floating u)
                  => (u -> Vec2 u) -> Circle u -> Anchor u
runDisplaceCenter fn (Circle { circ_ctm    = ctm
                             , circ_radius = radius }) = 
    projectFromCtr (fn radius) ctm

-- Anchors look like they need ctx...

instance (Real u, Floating u) => CenterAnchor (Circle u) where
  center = runDisplaceCenter $ \_ -> V2 0 0 


instance (Real u, Floating u) => CardinalAnchor (Circle u) where
  north = runDisplaceCenter $ \r -> V2 0    r
  south = runDisplaceCenter $ \r -> V2 0  (-r)
  east  = runDisplaceCenter $ \r -> V2 r    0
  west  = runDisplaceCenter $ \r -> V2 (-r) 0


instance (Real u, Floating u) => CardinalAnchor2 (Circle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


instance (Real u, Floating u) => RadialAnchor (Circle u) where
  radialAnchor ang = runDisplaceCenter $ \r -> avec ang r




--------------------------------------------------------------------------------
-- Construction

-- | 'circle'  : @ radius -> Shape @
--
circle :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
       => u -> Shape Circle u
circle radius = makeShape (mkCircle radius) (mkCirclePath radius)
          


mkCircle :: InterpretUnit u => u -> LocThetaQuery u (Circle u)
mkCircle radius = promoteR2 $ \ctr theta -> 
    pure $ Circle { circ_ctm    = makeShapeCTM ctr theta
                  , circ_radius = radius 
                  }


-- Rotation (theta) can be ignored.
--
mkCirclePath :: (Floating u, Ord u, InterpretUnit u, LengthTolerance u)
             => u -> LocThetaQuery u (AbsPath u)
mkCirclePath radius = promoteR2 $ \ctr _ -> 
    pure $ curvePath $ bezierCircle radius ctr 




