{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Parallelogram
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Parallelogram.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Parallelogram
  ( 

    Parallelogram
  , DParallelogram
  , parallelogram
  , zparallelogram


  ) where

import Wumpus.Drawing.Basis.Geometry
import Wumpus.Drawing.Basis.ShapeTrails
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.Intersection
import Wumpus.Drawing.Shapes.Base


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative




--------------------------------------------------------------------------------
-- Parallelogram

-- | A Paralleogram.
--
data Parallelogram u = Parallelogram 
      { pll_ctm             :: ShapeCTM u
      , pll_base_width      :: !u
      , pll_height          :: !u
      , pll_base_left_ang   :: Radian
      }

type instance DUnit (Parallelogram u) = u


type DParallelogram = Parallelogram Double


instance Functor Parallelogram where
  fmap f (Parallelogram ctm bw h lang) = 
      Parallelogram (fmap f ctm) (f bw) (f h) lang



--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Parallelogram u -> Parallelogram u
mapCTM f = (\s i -> s { pll_ctm = f i }) <*> pll_ctm


instance (Real u, Floating u) => Rotate (Parallelogram u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Parallelogram u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Parallelogram u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Parallelogram u) where
  translate dx dy       = mapCTM (translate dx dy)

--------------------------------------------------------------------------------
-- Anchors

-- | 'runDisplaceCenter' : @ ( base_width
--                           * height 
--                           * base_minor
--                           -> Vec ) * parallelogram -> Point @
--
runDisplaceCenter :: (Real u, Floating u)
                  => (u -> u -> Radian -> Vec2 u) -> Parallelogram u -> Anchor u
runDisplaceCenter fn (Parallelogram { pll_ctm           = ctm
                                    , pll_base_width    = bw
                                    , pll_height        = h 
                                    , pll_base_left_ang = lang }) =
    projectFromCtr (fn bw h lang) ctm


runDisplaceCenterHalves :: (Real u, Floating u)
                        => (u -> u -> Radian -> Vec2 u) 
                        -> Parallelogram u 
                        -> Anchor u
runDisplaceCenterHalves fn = 
    runDisplaceCenter $ \bw h bl_ang -> fn (0.5*bw) (0.5*h) bl_ang


instance (Real u, Floating u) => 
    CenterAnchor (Parallelogram u) where
  center = runDisplaceCenter $ \_ _ _ -> V2 0 0


-- | WARNING - WRONG...

-- top anchors swap the base minor and major...
--

instance (Real u, Floating u) => 
    TopCornerAnchor (Parallelogram u) where
  topLeftCorner  = runDisplaceCenterHalves $ \hw hh lang -> 
      let hypo = hh / (fromRadian $ sin lang) in hvec (-hw) ^+^ avec lang hypo

  topRightCorner = runDisplaceCenterHalves $ \hw hh lang ->
      let hypo = hh / (fromRadian $ sin lang) in hvec hw ^+^ avec lang hypo

instance (Real u, Floating u) => 
    BottomCornerAnchor (Parallelogram u) where
  bottomLeftCorner  = runDisplaceCenterHalves $ \hw hh lang ->
      let hypo = hh / (fromRadian $ sin lang) in hvec (-hw) ^+^ avec lang (-hypo)

  bottomRightCorner = runDisplaceCenterHalves $ \hw hh lang -> 
      let hypo = hh / (fromRadian $ sin lang) in hvec hw ^+^ avec lang (-hypo)



instance (Real u, Floating u) => 
    SideMidpointAnchor (Parallelogram u) where
  sideMidpoint n a = step (n `mod` 4) 
    where
      step 1 = midpoint (topRightCorner a)    (topLeftCorner a)
      step 2 = midpoint (topLeftCorner a)     (bottomLeftCorner a)
      step 3 = midpoint (bottomLeftCorner a)  (bottomRightCorner a)
      step _ = midpoint (bottomRightCorner a) (topRightCorner a)



instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    CardinalAnchor (Parallelogram u) where
  north = radialAnchor (0.5 * pi)
  south = radialAnchor (1.5 * pi)
  east  = radialAnchor 0
  west  = radialAnchor pi


instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    CardinalAnchor2 (Parallelogram u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)



instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
     RadialAnchor (Parallelogram u) where
   radialAnchor ang = runDisplaceCenter $ \bw h bl_ang-> 
      maybe zeroVec id $ pllRadialAnchor bw h bl_ang ang



-- | Note - it is not worth changing this to a quadrantAlg.
--
-- There are pathological parallelograms that the current 
-- QuadrantAlg code cannot handle, and a better abstraction is
-- needed (rather than better implementation of QuadrantAlg).
--
pllRadialAnchor :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                => u -> u -> Radian -> Radian -> Maybe (Vec2 u)
pllRadialAnchor bw h bl_ang ang =
    fmap (pvec zeroPt) $ rayPathIntersection (inclinedRay zeroPt ang) rp 
  where
    rp = anaTrailPath zeroPt $ parallelogram_trail bw h bl_ang


--------------------------------------------------------------------------------
-- Construction


-- | 'parallelogram'  : @ width * height * bottom_left_ang -> Parallelogram @
--
--
parallelogram :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
              => u -> u -> Radian -> Shape Parallelogram u
parallelogram bw h lang =
    makeShape (mkParallelogram bw h lang) (mkParallelogramPath bw h lang)


-- | 'zparallelogram'  : @ base_width * height -> Parallelogram @
--
--
zparallelogram :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
              => u -> u -> Shape Parallelogram u
zparallelogram bw h = parallelogram bw h ang
  where
    ang = d2r (60::Double)


--------------------------------------------------------------------------------


mkParallelogram :: (Real u, Fractional u, InterpretUnit u, Tolerance u) 
                => u -> u -> Radian -> LocThetaQuery u (Parallelogram u)
mkParallelogram bw h bl_ang = qpromoteLocTheta $ \ctr theta -> 
    pure $ Parallelogram { pll_ctm            = makeShapeCTM ctr theta
                         , pll_base_width     = bw
                         , pll_height         = h
                         , pll_base_left_ang  = bl_ang
                         }




mkParallelogramPath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                    => u -> u -> Radian -> LocThetaQuery u (AbsPath u)
mkParallelogramPath bw h bl_ang = qpromoteLocTheta $ \ctr theta -> 
    return $ anaTrailPath ctr $ rparallelogram_trail bw h bl_ang theta

                         




