{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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

import Data.AffineSpace                         -- package: vector-space 

import Control.Applicative



--------------------------------------------------------------------------------
-- Circle

data Circle u = Circle 
      { circ_ctm    :: ShapeCTM u
      , circ_radius :: !u 
      }
  deriving (Eq,Show)
  
type DCircle = Circle Double

type instance DUnit (Circle u) = u




mapCircleCTM :: (ShapeCTM u -> ShapeCTM u) -> Circle u -> Circle u
mapCircleCTM f = (\s i -> s { circ_ctm = f i }) <*> circ_ctm

instance Num u => Scale (Circle u) where
  scale sx sy = mapCircleCTM (scale sx sy)


instance Rotate (Circle u) where
  rotate ang = mapCircleCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Circle u) where
  rotateAbout ang pt = mapCircleCTM (rotateAbout ang pt)


instance Num u => Translate (Circle u) where
  translate dx dy = mapCircleCTM (translate dx dy)



runCircle :: (u -> ShapeCTM u -> a) -> Circle u -> a
runCircle fn (Circle { circ_ctm = ctm, circ_radius = radius }) = 
    fn radius ctm


instance (Real u, Floating u) => CenterAnchor (Circle u) where
  center = runCircle (\_ -> ctmCenter)


instance (Real u, Floating u) => CardinalAnchor (Circle u) where
  north = runCircle $ \r -> projectPoint $ P2 0    r
  south = runCircle $ \r -> projectPoint $ P2 0  (-r)
  east  = runCircle $ \r -> projectPoint $ P2 r    0
  west  = runCircle $ \r -> projectPoint $ P2 (-r) 0


instance (Real u, Floating u) => CardinalAnchor2 (Circle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


instance (Real u, Floating u) => RadialAnchor (Circle u) where
  radialAnchor theta = runCircle $ \r -> projectPoint $ zeroPt .+^ avec theta r






-- | 'circle'  : @ radius -> shape @
--
circle :: (Real u, Floating u) => u -> LocShape u (Circle u)
circle radius = intoLocShape (mkCircle radius) (mkCirclePath radius)
          


mkCircle :: Num u => u -> LocCF u (Circle u)
mkCircle radius = promoteR1 $ \ctr -> 
    pure $ Circle { circ_ctm = makeShapeCTM ctr, circ_radius = radius }



mkCirclePath :: (Floating u, Ord u) => u -> LocCF u (Path u)
mkCirclePath radius = promoteR1 $ \ctr -> 
    pure $ traceCurvePoints $ bezierCircle 2 radius ctr 



