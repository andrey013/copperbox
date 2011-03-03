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

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative



--------------------------------------------------------------------------------
-- Circle

data Circle u = Circle 
      { circ_ctm    :: ShapeCTM u
      , circ_radius :: !u 
      }
  
type DCircle = Circle Double



--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Circle u -> Circle u
mapCTM f = (\s i -> s { circ_ctm = f i }) <*> circ_ctm

instance PtSize u => Scale (Circle u) where
  scale sx sy = mapCTM (scale sx sy)


instance Rotate (Circle u) where
  rotate ang = mapCTM (rotate ang)
                  

instance (Real u, Floating u, PtSize u) => RotateAbout (Circle u) where
  rotateAbout ang pt = mapCTM (rotateAbout ang pt)


instance PtSize u => Translate (Circle u) where
  translate dx dy = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runDisplaceCenter :: (Real u, Floating u, PtSize u) 
                  => (u -> Vec2 u) -> Circle u -> Point2 u
runDisplaceCenter fn (Circle { circ_ctm    = ctm
                             , circ_radius = radius }) = 
    displaceCenter (fn radius) ctm



instance (Real u, Floating u, PtSize u) => CenterAnchor (Circle u) u where
  center = runDisplaceCenter $ \_ -> V2 0 0 


instance (Real u, Floating u, PtSize u) => CardinalAnchor (Circle u) u where
  north = runDisplaceCenter $ \r -> V2 0    r
  south = runDisplaceCenter $ \r -> V2 0  (-r)
  east  = runDisplaceCenter $ \r -> V2 r    0
  west  = runDisplaceCenter $ \r -> V2 (-r) 0


instance (Real u, Floating u, PtSize u) => CardinalAnchor2 (Circle u) u where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


instance (Real u, Floating u, PtSize u) => RadialAnchor (Circle u) u where
  radialAnchor ang = runDisplaceCenter $ \r -> avec ang r




--------------------------------------------------------------------------------
-- Construction

-- | 'circle'  : @ radius -> Shape @
--
circle :: (Real u, Floating u, PtSize u) 
       => u -> Shape Circle u
circle radius = makeShape (mkCircle radius) (mkCirclePath radius)
          


mkCircle :: Num u => u -> LocThetaCF u (Circle u)
mkCircle radius = promoteR2 $ \ctr theta -> 
    pure $ Circle { circ_ctm    = makeShapeCTM ctr theta
                  , circ_radius = radius 
                  }


-- Rotation (theta) can be ignored.
--
mkCirclePath :: (Floating u, Ord u, PtSize u) 
             => u -> LocThetaCF u (Path u)
mkCirclePath radius = promoteR2 $ \ctr _ -> 
    pure $ traceCurvePoints $ bezierCircle radius ctr 




