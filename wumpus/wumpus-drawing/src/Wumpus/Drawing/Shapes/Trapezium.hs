{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Trapezium
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Isoceles Trapezium.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Trapezium
  ( 

    Trapezium
  , DTrapezium
  , trapezium


  ) where

import Wumpus.Drawing.Basis.ShapeTrails
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.Intersection
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core



import Control.Applicative




--------------------------------------------------------------------------------
-- Trapezium

-- | A trapezium.
--
data Trapezium u = Trapezium 
      { tz_ctm              :: ShapeCTM u
      , tz_base_width       :: !u
      , tz_top_width        :: !u
      , tz_height           :: !u
      , tz_bottom_left_ang  :: Radian 
      }

type instance DUnit (Trapezium u) = u

type DTrapezium = Trapezium Double

instance Functor Trapezium where
  fmap f (Trapezium ctm bw tw h ang) = 
    Trapezium (fmap f ctm) (f bw) (f tw) (f h) ang

--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Trapezium u -> Trapezium u
mapCTM f = (\s i -> s { tz_ctm = f i }) <*> tz_ctm


instance (Real u, Floating u) => Rotate (Trapezium u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Trapezium u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Trapezium u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Trapezium u) where
  translate dx dy       = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors


-- | 'runDisplaceCenter' : @ ( base_width 
--                           * height
--                           * bl_ang -> Vec ) * trapezium -> Point @
--
runDisplaceCenter :: (Real u, Floating u)
                  => (u -> u -> Radian -> Vec2 u) 
                  -> Trapezium u -> Anchor u
runDisplaceCenter fn (Trapezium { tz_ctm              = ctm
                                , tz_base_width       = bw
                                , tz_height           = h 
                                , tz_bottom_left_ang  = bl_ang  }) =
    projectFromCtr (fn bw h bl_ang) ctm


-- | 'runDisplaceCenterHalves' : @ ( half_base_width 
--                                 * half_top_width
--                                 * half_height -> Vec ) * trapezium -> Point @
--
runDisplaceCenterHalves :: (Real u, Floating u)
                        => (u -> u -> u -> Vec2 u) 
                        -> Trapezium u -> Anchor u
runDisplaceCenterHalves fn (Trapezium { tz_ctm          = ctm
                                      , tz_base_width   = bw
                                      , tz_top_width    = tw
                                      , tz_height       = h   }) =
    projectFromCtr (fn (0.5 * bw) (0.5 * tw) (0.5 * h)) ctm


instance (Real u, Floating u) => 
    CenterAnchor (Trapezium u) where
  center = runDisplaceCenter $ \_ _ _ -> V2 0 0



instance (Real u, Floating u) => 
    BottomCornerAnchor (Trapezium u) where
  bottomLeftCorner  = runDisplaceCenterHalves $ \hbw _ hh -> V2 (-hbw) (-hh)
  bottomRightCorner = runDisplaceCenterHalves $ \hbw _ hh -> V2  hbw   (-hh)





instance (Real u, Floating u) => 
    TopCornerAnchor (Trapezium u) where
  topLeftCorner  = runDisplaceCenterHalves $ \_ htw hh -> V2 (-htw) hh
  topRightCorner = runDisplaceCenterHalves $ \_ htw hh -> V2   htw  hh


instance (Real u, Floating u, Tolerance u) => 
    SideMidpointAnchor (Trapezium u) where
  sideMidpoint n a = step (n `mod` 4) 
    where
      step 1 = north a
      step 2 = west a
      step 3 = south a
      step _ = east a



instance (Real u, Floating u, Tolerance u) => 
    CardinalAnchor (Trapezium u) where
  north = radialAnchor half_pi
  south = radialAnchor (1.5 * pi)
  east  = radialAnchor 0
  west  = radialAnchor pi


instance (Real u, Floating u, Tolerance u) => 
    CardinalAnchor2 (Trapezium u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)



instance (Real u, Floating u, Tolerance u) => 
    RadialAnchor (Trapezium u) where
   radialAnchor ang = runDisplaceCenter $ \bw h bl_ang -> 
      maybe zeroVec id $ trapeziumRadialAnchor bw h bl_ang ang


-- 
trapeziumRadialAnchor :: (Real u, Floating u, Tolerance u) 
                      => u -> u -> Radian -> Radian -> Maybe (Vec2 u)
trapeziumRadialAnchor bw h bl_ang ang =
    fmap (pvec zeroPt) $ rayPathIntersection (inclinedRay zeroPt ang) rp 
  where
    rp = anaTrailPath zeroPt $ trapezium_trail bw h bl_ang


    
--------------------------------------------------------------------------------
-- Construction


-- | 'trapezium'  : @ base_width * height * bottom_left_ang * 
--     bottom_right_ang -> Shape @
--
--
trapezium :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
          => u -> u -> Radian -> Shape Trapezium u
trapezium bw h base_ang = 
    makeShape (mkTrapezium bw h base_ang) (mkTrapeziumPath bw h base_ang)




--------------------------------------------------------------------------------


mkTrapezium :: (Real u, Fractional u, InterpretUnit u) 
            => u -> u -> Radian -> LocThetaQuery u (Trapezium u)
mkTrapezium bw h base_ang = qpromoteLocTheta $ \ctr theta -> 
    pure $ Trapezium { tz_ctm             = makeShapeCTM ctr theta
                     , tz_base_width      = bw
                     , tz_top_width       = tw
                     , tz_height          = h
                     , tz_bottom_left_ang = base_ang
                     }
  where
    base_minor = h / (fromRadian $ tan base_ang)
    tw         = bw - (2 * base_minor)


mkTrapeziumPath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                => u -> u -> Radian -> LocThetaQuery u (AbsPath u)
mkTrapeziumPath bw h bl_ang = qpromoteLocTheta $ \ctr theta -> 
    return $ anaTrailPath ctr $ rtrapezium_trail bw h bl_ang theta


