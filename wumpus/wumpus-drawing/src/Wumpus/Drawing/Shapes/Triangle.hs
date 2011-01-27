{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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

import Wumpus.Drawing.Geometry.Quadrant
import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




-- Datatype

-- | An isosceles triangle, oriented /upwards/.
--
data Triangle u = Triangle 
      { tri_ctm         :: ShapeCTM u
      , tri_height      :: !u
      , tri_base_width  :: !u
      , tri_syn_props   :: SyntheticProps u
      }
      
data SyntheticProps u = SyntheticProps
      { tri_hmajor      :: u           
      , tri_hminor      :: u
      , tri_base_ang    :: Radian
      , tri_apex_ang    :: Radian
      }


type DTriangle = Triangle Double

type instance DUnit (Triangle u) = u


--------------------------------------------------------------------------------
-- Affine trans

mapTriangleCTM :: (ShapeCTM u -> ShapeCTM u) 
               -> Triangle u -> Triangle u
mapTriangleCTM f = (\s i -> s { tri_ctm = f i }) <*> tri_ctm

instance Num u => Scale (Triangle u) where
  scale sx sy = mapTriangleCTM (scale sx sy)


instance Rotate (Triangle u) where
  rotate ang = mapTriangleCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Triangle u) where
  rotateAbout ang pt = mapTriangleCTM (rotateAbout ang pt)


instance Num u => Translate (Triangle u) where
  translate dx dy = mapTriangleCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

runDisplaceCenter :: (Real u, Floating u)
                  => (u -> u -> u -> Radian -> Vec2 u) -> Triangle u -> Point2 u
runDisplaceCenter fn (Triangle { tri_ctm        = ctm
                               , tri_base_width = bw
                               , tri_syn_props  = syn }) =  
    displaceCenter (fn (0.5*bw) hminor hmajor base_ang) ctm
  where
    hminor   = tri_hminor syn        
    hmajor   = tri_hmajor syn
    base_ang = tri_base_ang syn



instance (Real u, Floating u) => CenterAnchor (Triangle u) where
  center = runDisplaceCenter $ \_ _ _ _ -> V2 0 0




-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u) => CardinalAnchor (Triangle u) where
  north = runDisplaceCenter $ \_   _    hmaj _    -> V2 0 hmaj
  south = runDisplaceCenter $ \_   hmin _    _    -> V2 0 (-hmin)
  east  = runDisplaceCenter $ \hbw hmin _    ang  -> findEast hbw hmin ang
  west  = runDisplaceCenter $ \hbw hmin _    ang  -> findWest hbw hmin ang


findEast :: Fractional u => u -> u -> Radian -> Vec2 u
findEast half_base_width hminor base_ang = V2 xdist 0
  where
    b1    = hminor / (fromRadian $ tan base_ang)
    xdist = half_base_width - b1

findWest :: Fractional u => u -> u -> Radian -> Vec2 u
findWest hbw hm ang = let (V2 xdist 0) = findEast hbw hm ang in V2 (-xdist) 0 



instance (Real u, Floating u) => CardinalAnchor2 (Triangle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)



instance (Real u, Floating u) => RadialAnchor (Triangle u) where
  radialAnchor theta = runDisplaceCenter $ \hbw hmin hmaj _ -> 
                         triangleRadialVector hbw hmin hmaj theta
       
    
--------------------------------------------------------------------------------
-- Construction

-- | 'triangle'  : @ base_width * height -> Shape @
--
--
triangle :: (Real u, Floating u, FromPtSize u) 
         => u -> u -> Shape u (Triangle u)
triangle bw h =
    let props  = synthesizeProps bw h
        hminor = tri_hminor props
        hmajor = tri_hmajor props
    in makeShape (mkTriangle bw h props) (mkTrianglePath bw hminor hmajor)





mkTriangle :: (Real u, Fractional u) 
           => u -> u -> SyntheticProps u -> LocThetaCF u (Triangle u)
mkTriangle bw h props = promoteR2 $ \ctrd theta -> 
    pure $ Triangle { tri_ctm        = makeShapeCTM ctrd theta
                    , tri_base_width = bw
                    , tri_height     = h 
                    , tri_syn_props  = props
                    }


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



mkTrianglePath :: (Real u, Floating u, FromPtSize u) 
               => u -> u -> u -> LocThetaCF u (Path u)
mkTrianglePath bw hminor hmajor = promoteR2 $ \ctr theta -> 
    roundCornerShapePath $ map (rotateAbout theta ctr) 
                         $ trianglePath bw hminor hmajor ctr


trianglePath :: (Real u, Floating u) 
             => u -> u -> u -> LocCoordPath u
trianglePath bw hminor hmajor (P2 x y) = [br, apex, bl]
  where
    half_base = 0.5 * bw
    br        = P2 (x + half_base ) (y - hminor)
    apex      = P2 x (y + hmajor)
    bl        = P2 (x - half_base ) (y - hminor)
