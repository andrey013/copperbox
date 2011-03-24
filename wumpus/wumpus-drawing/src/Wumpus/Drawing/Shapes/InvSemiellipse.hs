{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
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



instance Functor InvSemiellipse where
  fmap f = InvSemiellipse . fmap f . getInvSemiellipse

--------------------------------------------------------------------------------
-- Affine trans

mapInner :: (Semiellipse u -> Semiellipse u) 
         -> InvSemiellipse u 
         -> InvSemiellipse u
mapInner f = InvSemiellipse . f . getInvSemiellipse

instance InterpretUnit u => CtxRotate InvSemiellipse u where
  ctxRotate sz ang = mapInner (ctxRotate sz ang)
              
instance InterpretUnit u => CtxRotateAbout InvSemiellipse u where
  ctxRotateAbout sz ang pt = mapInner (ctxRotateAbout sz ang pt)

instance InterpretUnit u => CtxScale InvSemiellipse u where
  ctxScale sz sx sy = mapInner (ctxScale sz sx sy)

instance InterpretUnit u => CtxTranslate InvSemiellipse u where
  ctxTranslate sz dx dy = mapInner (ctxTranslate sz dx dy)


--------------------------------------------------------------------------------
-- Anchors

runRotateAnchor :: (Real u, Floating u, InterpretUnit u) 
                => (Semiellipse u -> Anchor u) -> InvSemiellipse u -> Anchor u
runRotateAnchor f (InvSemiellipse a) =
   center a  >>= \ctr -> 
   f a       >>= \a1 -> 
   pointSize >>= \sz -> 
   return $ ctxRotateAbout sz pi ctr a1



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CenterAnchor InvSemiellipse u where
  center = center . getInvSemiellipse

instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    ApexAnchor InvSemiellipse u where
  apex = runRotateAnchor apex

instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    TopCornerAnchor InvSemiellipse u where
  topLeftCorner  = runRotateAnchor bottomRightCorner
  topRightCorner = runRotateAnchor bottomLeftCorner

instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor InvSemiellipse u where
  north = runRotateAnchor south
  south = runRotateAnchor north
  east  = runRotateAnchor west
  west  = runRotateAnchor east


instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor2 InvSemiellipse u where
  northeast = runRotateAnchor southwest
  southeast = runRotateAnchor northwest
  southwest = runRotateAnchor northeast
  northwest = runRotateAnchor southeast



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    RadialAnchor InvSemiellipse u where
  radialAnchor theta = 
    runRotateAnchor (radialAnchor $ circularModulo $ pi+theta)


--------------------------------------------------------------------------------
-- Construction

-- | 'invsemiellipse'  : @ rx * ry -> Shape @
--
invsemiellipse :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
           => u -> u -> Shape InvSemiellipse u
invsemiellipse rx ry = 
    shapeMap InvSemiellipse $ updatePathAngle (+ pi) $ semiellipse rx ry
