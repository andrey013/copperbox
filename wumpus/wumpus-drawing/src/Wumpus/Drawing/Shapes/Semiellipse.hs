{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Semiellipse
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Semiellipse.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Semiellipse
  ( 

    Semiellipse
  , DSemiellipse
  , semiellipse

  ) where

import Wumpus.Drawing.Basis.ShapeTrails
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.Intersection
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative



--------------------------------------------------------------------------------
-- Datatype

data Semiellipse u = Semiellipse 
      { se_ctm          :: ShapeCTM u
      , se_rx           :: !u 
      , se_ry           :: !u
      }

type instance DUnit (Semiellipse u) = u


  
type DSemiellipse = Semiellipse Double

instance Functor Semiellipse where
  fmap f (Semiellipse ctm rx ry) = Semiellipse (fmap f ctm) (f rx) (f ry)



ryminor :: Floating u => u -> u
ryminor ry = (4 * ry) / (3 * pi)

rymajor :: Floating u => u -> u
rymajor ry = ry - ryminor ry


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Semiellipse u -> Semiellipse u
mapCTM f = (\s i -> s { se_ctm = f i }) <*> se_ctm


instance (Real u, Floating u) => Rotate (Semiellipse u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Semiellipse u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Semiellipse u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Semiellipse u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors


-- | 'runDisplaceCenter' : @ ( rx
--                           * ry 
--                           * ry_minor 
--                           * ry_major -> Vec ) * semiellipse -> Point @
--
runDisplaceCenter :: (Real u, Floating u) 
                  => (u -> u -> Vec2 u) -> Semiellipse u -> Anchor u
runDisplaceCenter fn (Semiellipse { se_ctm       = ctm
                                  , se_rx        = rx
                                  , se_ry        = ry  }) = 
    projectFromCtr (fn rx ry) ctm




instance (Real u, Floating u) => 
    CenterAnchor (Semiellipse u) where
  center = runDisplaceCenter $ \_ _ -> V2 0 0

instance (Real u, Floating u, Tolerance u) => 
    ApexAnchor (Semiellipse u) where
  apex = runDisplaceCenter $ \_ ry -> V2 0 (rymajor ry)

instance (Real u, Floating u) => 
    BottomCornerAnchor (Semiellipse u) where
  bottomLeftCorner  = runDisplaceCenter $ \rx ry -> V2 (-rx) (negate $ ryminor ry)
  bottomRightCorner = runDisplaceCenter $ \rx ry -> V2  rx   (negate $ ryminor ry)


instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    CardinalAnchor (Semiellipse u) where
  north = apex
  south = runDisplaceCenter $ \_ ry -> V2 0 (negate $ ryminor ry)
  east  = radialAnchor 0
  west  = radialAnchor pi

instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    CardinalAnchor2 (Semiellipse u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)



instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    RadialAnchor (Semiellipse u) where
  radialAnchor ang = runDisplaceCenter $ \rx ry ->
      maybe zeroVec id $ semiellipseRadialAnchor rx ry ang


semiellipseRadialAnchor :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                        => u -> u -> Radian -> Maybe (Vec2 u) 
semiellipseRadialAnchor rx ry ang = 
    fmap (pvec zeroPt) $ rayPathIntersection (inclinedRay zeroPt ang) rp 
  where
    rp = anaTrailPath zeroPt $ semiellipse_trail rx ry


--------------------------------------------------------------------------------
-- Construction


-- | 'semiellipse'  : @ x_radius * y_radius -> Shape @
--
semiellipse :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
            => u -> u -> Shape Semiellipse u
semiellipse rx ry = makeShape (mkSemiellipse rx ry) (mkSemiellipsePath rx ry)
          



mkSemiellipse :: InterpretUnit u 
              => u -> u -> LocThetaQuery u (Semiellipse u)
mkSemiellipse rx ry = qpromoteLocTheta $ \ctr theta -> 
    pure $ Semiellipse { se_ctm = makeShapeCTM ctr theta
                       , se_rx = rx
                       , se_ry = ry
                       }


mkSemiellipsePath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                  => u -> u -> LocThetaQuery u (AbsPath u)
mkSemiellipsePath rx ry = qpromoteLocTheta $ \ctr theta ->
    return $ anaTrailPath ctr $ rsemiellipse_trail rx ry theta

