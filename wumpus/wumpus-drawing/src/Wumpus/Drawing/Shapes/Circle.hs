{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
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

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



--------------------------------------------------------------------------------
-- Circle

data Circle u = Circle 
      { circ_ctm    :: ShapeCTM
      , circ_radius :: !u 
      }
  
type DCircle = Circle Double



--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM -> ShapeCTM) -> Circle u -> Circle u
mapCTM f = (\s i -> s { circ_ctm = f i }) <*> circ_ctm

instance Scale (Circle u) where
  scale sx sy = mapCTM (scale sx sy)


instance Rotate (Circle u) where
  rotate ang = mapCTM (rotate ang)
                  

instance RotateAbout (Circle u) where
  rotateAbout ang pt = mapCTM (rotateAbout ang pt)


instance Translate (Circle u) where
  translate dx dy = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runDisplaceCenter :: InterpretUnit u
                  => (u -> Vec2 u) -> Circle u -> Anchor u
runDisplaceCenter fn (Circle { circ_ctm    = ctm
                             , circ_radius = radius }) = 
    displaceCenter (fn radius) ctm

-- Anchors look like they need ctx...

instance InterpretUnit u => CenterAnchor Circle u where
  center = runDisplaceCenter $ \_ -> V2 0 0 


instance InterpretUnit u => CardinalAnchor Circle u where
  north = runDisplaceCenter $ \r -> V2 0    r
  south = runDisplaceCenter $ \r -> V2 0  (-r)
  east  = runDisplaceCenter $ \r -> V2 r    0
  west  = runDisplaceCenter $ \r -> V2 (-r) 0


instance (Floating u, InterpretUnit u) => CardinalAnchor2 Circle u where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


instance (Floating u, InterpretUnit u) => RadialAnchor Circle u where
  radialAnchor ang = runDisplaceCenter $ \r -> avec ang r




--------------------------------------------------------------------------------
-- Construction

-- | 'circle'  : @ radius -> Shape @
--
circle :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
       => u -> Shape Circle u
circle radius = makeShape (mkCircle radius) (mkCirclePath radius)
          


mkCircle :: InterpretUnit u => u -> LocThetaQuery u (Circle u)
mkCircle radius = promoteQ2 $ \ctr theta -> 
    info (uconvertExtQ ctr) >>= \dctr ->
    pure $ Circle { circ_ctm    = makeShapeCTM dctr theta
                  , circ_radius = radius 
                  }


-- Rotation (theta) can be ignored.
--
mkCirclePath :: (Floating u, Ord u, InterpretUnit u, LengthTolerance u)
             => u -> LocThetaQuery u (Path u)
mkCirclePath radius = promoteQ2 $ \ctr _ -> 
    pure $ traceCurvePoints $ bezierCircle radius ctr 




