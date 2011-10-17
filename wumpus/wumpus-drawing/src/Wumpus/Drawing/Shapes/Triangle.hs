{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Triangle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Isosceles triangle.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Triangle
  ( 

    Triangle
  , DTriangle
  , triangle

  ) where

import Wumpus.Drawing.Basis.ShapeTrails
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Paths.Intersection
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




-- Datatype

-- | An isosceles triangle, oriented /upwards/.
--
data Triangle u = Triangle 
      { tri_ctm         :: ShapeCTM u
      , tri_base_width  :: !u
      , tri_height      :: !u
      }

type instance DUnit (Triangle u) = u

type DTriangle = Triangle Double

instance Functor Triangle where
  fmap f (Triangle ctm bw h) = Triangle (fmap f ctm) (f bw) (f h)


hminor :: Fractional u => u -> u 
hminor h = h / 3

hmajor :: Fractional u => u -> u 
hmajor h = 2 * (h / 3)



--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Triangle u -> Triangle u
mapCTM f = (\s i -> s { tri_ctm = f i }) <*> tri_ctm


instance (Real u, Floating u) => Rotate (Triangle u) where
  rotate ang            = mapCTM (rotate ang)
              
instance (Real u, Floating u) => RotateAbout (Triangle u) where
  rotateAbout ang pt    = mapCTM (rotateAbout ang pt)

instance Fractional u => Scale (Triangle u) where
  scale sx sy           = mapCTM (scale sx sy)

instance InterpretUnit u => Translate (Triangle u) where
  translate dx dy       = mapCTM (translate dx dy)

--------------------------------------------------------------------------------
-- Anchors

-- | 'runDisplaceCenter' : @ ( half_base_width 
--                           * height_minor 
--                           * height_major 
--                           * base_ang -> Vec ) * traingle -> Point @
--
runDisplaceCenter :: (Real u, Floating u)
                  => (u -> u -> Vec2 u) -> Triangle u -> Anchor u
runDisplaceCenter fn (Triangle { tri_ctm        = ctm
                               , tri_base_width = bw 
                               , tri_height     = h   }) =  
    projectFromCtr (fn bw h) ctm


instance (Real u, Floating u) => 
    CenterAnchor (Triangle u) where
  center = runDisplaceCenter $ \_ _ -> V2 0 0


instance (Real u, Floating u) => 
    ApexAnchor (Triangle u) where
  apex = runDisplaceCenter $ \_ h -> V2 0 (hmajor h)


instance (Real u, Floating u) => 
    BottomCornerAnchor (Triangle u) where
  bottomLeftCorner  = runDisplaceCenter $ \bw h -> 
                        V2 (negate $ 0.5 * bw) (negate $ hminor h)
  bottomRightCorner = runDisplaceCenter $ \bw h -> 
                        V2 (0.5 * bw)          (negate $ hminor h)


-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u) => 
    CardinalAnchor (Triangle u) where
  north = runDisplaceCenter $ \_  h -> V2 0 (hmajor h)
  south = runDisplaceCenter $ \_  h -> V2 0 (negate $ hminor h)
  east  = runDisplaceCenter $ \bw h -> findEast bw h
  west  = runDisplaceCenter $ \bw h -> findWest bw h


instance (Real u, Floating u) => 
    SideMidpointAnchor (Triangle u) where
  sideMidpoint n a = step (n `mod` 3) 
    where
      step 1 = midpoint (apex a)              (bottomLeftCorner a)
      step 2 = midpoint (bottomLeftCorner a)  (bottomRightCorner a)
      step _ = midpoint (bottomRightCorner a) (apex a)


findEast :: (Real u, Fractional u) => u -> u -> Vec2 u
findEast bw h = V2 xdist 0
  where
    half_base   = 0.5 * bw 
    base_ang    = atan $ toRadian (h / half_base)
    b1          = (hminor h) / (fromRadian $ tan base_ang)
    xdist       = (0.5 * bw) - b1

findWest :: (Real u, Fractional u) => u -> u -> Vec2 u
findWest bw h = let (V2 xdist 0) = findEast bw h in V2 (-xdist) 0 



instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    CardinalAnchor2 (Triangle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)



instance (Real u, Floating u, InterpretUnit u, Tolerance u) => 
    RadialAnchor (Triangle u) where
  radialAnchor ang = runDisplaceCenter $ \bw h -> 
      maybe zeroVec id $ triangleRadialAnchor bw h ang



triangleRadialAnchor :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
                      => u -> u -> Radian -> Maybe (Vec2 u) 
triangleRadialAnchor bw h ang = 
    fmap (pvec zeroPt) $ rayPathIntersection (inclinedRay zeroPt ang) rp 
  where
    rp = anaTrailPath zeroPt $ isosceles_triangle_trail bw h

    
--------------------------------------------------------------------------------
-- Construction

-- | 'triangle'  : @ base_width * height -> Shape @
--
--
triangle :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
         => u -> u -> Shape Triangle u
triangle bw h = makeShape (mkTriangle bw h) (mkTrianglePath bw h)





mkTriangle :: (Real u, Fractional u, InterpretUnit u) 
           => u -> u -> LocThetaQuery u (Triangle u)
mkTriangle bw h = qpromoteLocTheta $ \ctrd theta -> 
    pure $ Triangle { tri_ctm        = makeShapeCTM ctrd theta
                    , tri_base_width = bw
                    , tri_height     = h 
                    }





mkTrianglePath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
               => u -> u -> LocThetaQuery u (AbsPath u)
mkTrianglePath bw h = qpromoteLocTheta $ \ctr theta -> 
    return $ anaTrailPath ctr $ risosceles_triangle_trail bw h theta

