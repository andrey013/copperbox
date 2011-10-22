{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Semicircle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Semicircle. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Semicircle
  ( 

    Semicircle
  , DSemicircle
  , semicircle

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

data Semicircle u = Semicircle 
      { sc_ctm          :: ShapeCTM u
      , sc_radius       :: !u 
      }

type instance DUnit (Semicircle u) = u

  
type DSemicircle = Semicircle Double


instance Functor Semicircle where
  fmap f (Semicircle ctm r) = Semicircle (fmap f ctm) (f r)



-- | Use the formula:
--
-- >   4r
-- >  ---
-- >  3pi
--
-- to get the yminor.
--


hminor :: Floating u => u -> u
hminor radius = (4 * radius) / (3 * pi)

hmajor :: Floating u => u -> u
hmajor radius = radius - hminor radius


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Semicircle u -> Semicircle u
mapCTM f = (\s i -> s { sc_ctm = f i }) <*> sc_ctm


instance (Real u, Floating u) => Rotate (Semicircle u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Semicircle u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Semicircle u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Semicircle u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- | 'runDisplaceCenter' : @ ( radius
--                           * height_minor 
--                           * height_major -> Vec ) * semicircle -> Point @
--
runDisplaceCenter :: (Real u, Floating u) 
                  => (u -> Vec2 u) -> Semicircle u -> Anchor u
runDisplaceCenter fn (Semicircle { sc_ctm       = ctm
                                 , sc_radius    = radius }) = 
    projectFromCtr (fn radius) ctm


instance (Real u, Floating u) => 
    CenterAnchor (Semicircle u) where
  center = runDisplaceCenter $ \_ -> V2 0 0

instance (Real u, Floating u) => 
    ApexAnchor (Semicircle u) where
  apex = runDisplaceCenter $ \r -> V2 0 (hmajor r)

instance (Real u, Floating u) => 
    BottomCornerAnchor (Semicircle u) where
  bottomLeftCorner  = runDisplaceCenter $ \r -> V2 (-r) (negate $ hminor r)
  bottomRightCorner = runDisplaceCenter $ \r -> V2  r   (negate $ hminor r)

instance (Real u, Floating u) => 
    CardinalAnchor (Semicircle u) where
  north = apex
  south = runDisplaceCenter $ \r -> V2 0  (negate $ hminor r)
  east  = runDisplaceCenter $ \r -> let x = pyth r (hminor r) in V2 x 0
  west  = runDisplaceCenter $ \r -> let x = pyth r (hminor r) in V2 (-x) 0

-- | Use Pythagoras formula for working out the /east/ and /west/
-- distances. A right-triangle is formed below the centroid, 
-- radius is the hypotenuese, hminor is the other side.
--
pyth :: Floating u => u -> u -> u
pyth hyp s1 = sqrt $ pow2 hyp - pow2 s1
  where
    pow2 = (^ (2::Int))


instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    CardinalAnchor2 (Semicircle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)




instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    RadialAnchor (Semicircle u) where
  radialAnchor ang = runDisplaceCenter $ \r ->
      maybe zeroVec id $ semicircleRadialAnchor r ang



semicircleRadialAnchor :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                    => u -> Radian -> Maybe (Vec2 u) 
semicircleRadialAnchor r ang = 
    fmap (pvec zeroPt) $ rayPathIntersection (inclinedRay zeroPt ang) rp 
  where
    rp = anaTrailPath zeroPt $ semicircle_trail r


--------------------------------------------------------------------------------
-- Construction

-- | 'semicircle'  : @ radius -> Shape @
--
semicircle :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
           => u -> Shape Semicircle u
semicircle radius = makeShape (mkSemicircle radius) (mkSemicirclePath radius)
          


mkSemicircle :: InterpretUnit u
             => u -> LocThetaQuery u (Semicircle u)
mkSemicircle radius = qpromoteLocTheta $ \ctr theta -> 
    pure $ Semicircle { sc_ctm    = makeShapeCTM ctr theta
                      , sc_radius = radius
                      }



-- TODO - need to check other shapes to see if the are deriving 
-- the center properly...
--
mkSemicirclePath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                 => u -> LocThetaQuery u (AbsPath u)
mkSemicirclePath radius = qpromoteLocTheta $ \ctr theta ->
    return $ anaTrailPath ctr $ rsemicircle_trail radius theta

