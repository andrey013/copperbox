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

import Wumpus.Drawing.Paths
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
      
data SyntheticProps u = SyntheticProps
      { tri_hmajor      :: u           
      , tri_hminor      :: u
      , tri_base_ang    :: Radian
      , tri_apex_ang    :: Radian
      }


type DTriangle = Triangle Double

instance Functor Triangle where
  fmap f (Triangle ctm bw h) = Triangle (fmap f ctm) (f bw) (f h)


synthesizeProps :: (Real u, Fractional u) => u -> u -> SyntheticProps u
synthesizeProps bw h = 
    SyntheticProps { tri_hmajor      = hmajor
                   , tri_hminor      = hminor
                   , tri_base_ang    = base_ang
                   , tri_apex_ang    = apex_ang
                   }
  where
    half_base   = 0.5 * bw 
    hminor      = h / 3
    hmajor      = 2 * hminor 
    base_ang    = atan $ toRadian (h / half_base)
    apex_ang    = 2 * ((pi/4) - base_ang)


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
                  => (u -> u -> u -> Radian -> Vec2 u) -> Triangle u -> Anchor u
runDisplaceCenter fn (Triangle { tri_ctm        = ctm
                               , tri_base_width = bw 
                               , tri_height     = h   }) =  
    projectFromCtr (fn (0.5*bw) hminor hmajor base_ang) ctm
  where
    props    = synthesizeProps bw h
    hminor   = tri_hminor props  
    hmajor   = tri_hmajor props
    base_ang = tri_base_ang props



instance (Real u, Floating u) => 
    CenterAnchor (Triangle u) where
  center = runDisplaceCenter $ \_ _ _ _ -> V2 0 0


instance (Real u, Floating u) => 
    ApexAnchor (Triangle u) where
  apex = runDisplaceCenter $ \_ _ hmaj _ -> V2 0 hmaj


instance (Real u, Floating u) => 
    BottomCornerAnchor (Triangle u) where
  bottomLeftCorner  = runDisplaceCenter $ \hbw hmin _ _  -> V2 (-hbw) (-hmin)
  bottomRightCorner = runDisplaceCenter $ \hbw hmin _ _  -> V2  hbw   (-hmin)


-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u) => 
    CardinalAnchor (Triangle u) where
  north = runDisplaceCenter $ \_   _    hmaj _    -> V2 0 hmaj
  south = runDisplaceCenter $ \_   hmin _    _    -> V2 0 (-hmin)
  east  = runDisplaceCenter $ \hbw hmin _    ang  -> findEast hbw hmin ang
  west  = runDisplaceCenter $ \hbw hmin _    ang  -> findWest hbw hmin ang


instance (Real u, Floating u) => 
    SideMidpointAnchor (Triangle u) where
  sideMidpoint n a = step (n `mod` 3) 
    where
      step 1 = midpoint (apex a)              (bottomLeftCorner a)
      step 2 = midpoint (bottomLeftCorner a)  (bottomRightCorner a)
      step _ = midpoint (bottomRightCorner a) (apex a)


findEast :: Fractional u => u -> u -> Radian -> Vec2 u
findEast half_base_width hminor base_ang = V2 xdist 0
  where
    b1    = hminor / (fromRadian $ tan base_ang)
    xdist = half_base_width - b1

findWest :: Fractional u => u -> u -> Radian -> Vec2 u
findWest hbw hm ang = let (V2 xdist 0) = findEast hbw hm ang in V2 (-xdist) 0 



instance (Real u, Floating u) => 
    CardinalAnchor2 (Triangle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)



instance (Real u, Floating u) => 
    RadialAnchor (Triangle u) where
  radialAnchor theta = runDisplaceCenter $ \hbw hmin hmaj _ -> 
                         triangleRadialVector hbw hmin hmaj theta
       
    
--------------------------------------------------------------------------------
-- Construction

-- | 'triangle'  : @ base_width * height -> Shape @
--
--
triangle :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
         => u -> u -> Shape Triangle u
triangle bw h =
    let props  = synthesizeProps bw h
        hminor = tri_hminor props
        hmajor = tri_hmajor props
    in makeShape (mkTriangle bw h) (mkTrianglePath 0 bw hminor hmajor)





mkTriangle :: (Real u, Fractional u, InterpretUnit u) 
           => u -> u -> LocThetaQuery u (Triangle u)
mkTriangle bw h = qpromoteLocTheta $ \ctrd theta -> 
    pure $ Triangle { tri_ctm        = makeShapeCTM ctrd theta
                    , tri_base_width = bw
                    , tri_height     = h 
                    }





mkTrianglePath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
               => u -> u -> u -> u -> LocThetaQuery u (AbsPath u)
mkTrianglePath rnd bw hminor hmajor = qpromoteLocTheta $ \ctr theta -> 
    let xs = runVertices3 ctr $ trianglePath bw hminor hmajor
    in roundCornerShapePath rnd $ map (rotateAbout theta ctr) xs



trianglePath :: (Real u, Floating u) 
             => u -> u -> u -> Vertices3 u
trianglePath bw hminor hmajor = (br, apx, bl)
  where
    half_base = 0.5 * bw
    br        = V2   half_base  (-hminor)
    apx       = V2   0            hmajor
    bl        = V2 (-half_base) (-hminor)

