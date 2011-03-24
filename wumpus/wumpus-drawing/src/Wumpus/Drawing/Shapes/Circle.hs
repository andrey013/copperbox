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

instance Functor Circle where
  fmap f (Circle ctm r) = Circle (fmap f ctm) (f r)


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Circle u -> Circle u
mapCTM f = (\s i -> s { circ_ctm = f i }) <*> circ_ctm



instance InterpretUnit u => CtxRotate Circle u where
  ctxRotate sz ang = mapCTM (ctxRotate sz ang)
                  

instance InterpretUnit u => CtxRotateAbout Circle u where
  ctxRotateAbout sz ang pt = mapCTM (ctxRotateAbout sz ang pt)

instance InterpretUnit u => CtxScale Circle u where
  ctxScale sz sx sy = mapCTM (ctxScale sz sx sy)

instance InterpretUnit u => CtxTranslate Circle u where
  ctxTranslate sz dx dy = mapCTM (ctxTranslate sz dx dy)


--------------------------------------------------------------------------------
-- Anchors

runDisplaceCenter :: InterpretUnit u
                  => (u -> Vec2 u) -> Circle u -> Anchor u
runDisplaceCenter fn (Circle { circ_ctm    = ctm
                             , circ_radius = radius }) = 
    projectFromCtr (fn radius) ctm

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
mkCircle radius = promoteR2 $ \ctr theta -> 
    pure $ Circle { circ_ctm    = makeShapeCTM ctr theta
                  , circ_radius = radius 
                  }


-- Rotation (theta) can be ignored.
--
mkCirclePath :: (Floating u, Ord u, InterpretUnit u, LengthTolerance u)
             => u -> LocThetaQuery u (Path u)
mkCirclePath radius = promoteR2 $ \ctr _ -> 
    pure $ curvePath $ bezierCircle radius ctr 




