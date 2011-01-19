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

    IsoscelesTriangle
  , DIsoscelesTriangle
  , isoscelesTriangle


  ) where

import Wumpus.Drawing.Geometry.Intersection
import Wumpus.Drawing.Geometry.Paths
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




--------------------------------------------------------------------------------
-- Diamond

-- | An isosceles triangle, oriented /upwards/.
--
data IsoscelesTriangle u = IsoscelesTriangle 
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


type DIsoscelesTriangle = IsoscelesTriangle Double

type instance DUnit (IsoscelesTriangle u) = u


--------------------------------------------------------------------------------
-- Affine trans

mapTriangleCTM :: (ShapeCTM u -> ShapeCTM u) 
               -> IsoscelesTriangle u -> IsoscelesTriangle u
mapTriangleCTM f = (\s i -> s { tri_ctm = f i }) <*> tri_ctm

instance Num u => Scale (IsoscelesTriangle u) where
  scale sx sy = mapTriangleCTM (scale sx sy)


instance Rotate (IsoscelesTriangle u) where
  rotate ang = mapTriangleCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (IsoscelesTriangle u) where
  rotateAbout ang pt = mapTriangleCTM (rotateAbout ang pt)


instance Num u => Translate (IsoscelesTriangle u) where
  translate dx dy = mapTriangleCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- 'runtriangle' : @ half_base_width * hminor * hmajor * base_ang *ctm -> Ans @
-- 
runTriangle :: Fractional u 
            => (u -> u -> u -> Radian -> ShapeCTM u  -> a) 
            -> IsoscelesTriangle u -> a
runTriangle fn (IsoscelesTriangle { tri_ctm        = ctm
                                  , tri_base_width = bw
                                  , tri_syn_props = syn  }) =  
    fn (0.5*bw) (tri_hminor syn) (tri_hmajor syn) (tri_base_ang syn) ctm



instance (Real u, Floating u) => CenterAnchor (IsoscelesTriangle u) where
  center = ctmCenter . tri_ctm




-- east and west should be parallel to the centroid.
--

instance (Real u, Floating u) => CardinalAnchor (IsoscelesTriangle u) where
  north = runTriangle $ \_   _    hmaj _    -> projectPoint $ P2 0 hmaj
  south = runTriangle $ \_   hmin _    _    -> projectPoint $ P2 0 (-hmin)
  east  = runTriangle $ \hbw hmin _    ang  -> projectPoint (findEast hbw hmin ang)
  west  = runTriangle $ \hbw hmin _    ang  -> projectPoint (findWest hbw hmin ang)


findEast :: Fractional u => u -> u -> Radian -> Point2 u
findEast half_base_width hminor base_ang = P2 xdist 0
  where
    b1    = hminor / (fromRadian $ tan base_ang)
    xdist = half_base_width - b1

findWest :: Fractional u => u -> u -> Radian -> Point2 u
findWest hbw hm ang = let (P2 xdist 0) = findEast hbw hm ang in P2 (-xdist) 0 



instance (Real u, Floating u) => CardinalAnchor2 (IsoscelesTriangle u) where
  northeast = triRadialAnchor (0.25*pi)
  southeast = triRadialAnchor (1.75*pi)
  southwest = triRadialAnchor (1.25*pi)
  northwest = triRadialAnchor (0.75*pi)



instance (Real u, Floating u) => RadialAnchor (IsoscelesTriangle u) where
   radialAnchor = triRadialAnchor


triRadialAnchor :: (Real u, Floating u) 
                => Radian -> IsoscelesTriangle u -> Point2 u
triRadialAnchor theta (IsoscelesTriangle { tri_ctm        = ctm
                                           , tri_base_width = bw
                                           , tri_syn_props  = syn }) = 
    maybe ctr id $ findIntersect ctr theta $ polygonLines ps
  where 
    ps  = trianglePoints bw (tri_hminor syn) (tri_hmajor syn) ctm 
    ctr = ctmCenter ctm
    
    

-- | 'isoscelesTriangle'  : @ base_width * height -> Triangle @
--
--
isoscelesTriangle :: (Real u, Floating u, FromPtSize u) 
                  => u -> u -> LocShape u (IsoscelesTriangle u)
isoscelesTriangle bw h = 
   let props  = synthesizeProps bw h
       hminor = tri_hminor props
       hmajor = tri_hmajor props
   in intoLocShape (mkTriangle bw h props) (mkTrianglePath bw hminor hmajor)




mkTriangle :: (Real u, Fractional u) 
           => u -> u -> SyntheticProps u -> LocCF u (IsoscelesTriangle u)
mkTriangle bw h props = promoteR1 $ \ctrd -> 
    pure $ IsoscelesTriangle { tri_ctm        = makeShapeCTM ctrd
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
    base_ang    = atan $ toRadian (h / half_base)
    apex_ang    = 2 * ((pi/4) - base_ang)
    hminor      = half_base * (fromRadian $ tan (base_ang / 2))
    hmajor      = h - hminor

    -- base_ang - use trigonometry
    -- apex_ang - consider a half right triangle, sum all angles 
    --            is 180, subtract 90 for the right angle, so 
    --            the equation for half the angle is (90deg - base).
    --            Final step is to double the half angle.
    


mkTrianglePath :: (Real u, Floating u, FromPtSize u) 
               => u -> u -> u -> LocCF u (Path u)
mkTrianglePath bw hminor hmajor = promoteR1 $ \ctr -> 
    roundCornerShapePath $ trianglePath bw hminor hmajor ctr


trianglePath :: (Real u, Floating u) => u -> u -> u -> LocCoordPath u
trianglePath bw hminor hmajor (P2 x y) = [br, apex , bl]
  where
    half_base = 0.5 * bw
    br        = P2 (x + half_base ) (y - hminor)
    apex      = P2 x (y + hmajor)
    bl        = P2 (x - half_base ) (y - hminor)


trianglePoints :: (Real u, Floating u) 
               => u -> u -> u -> ShapeCTM u -> [Point2 u]
trianglePoints bw hminor hmajor ctm = map (projectPoint `flip` ctm) [ br, apex, bl ]
  where
    half_base = 0.5 * bw
    br        = P2 half_base    (-hminor)
    apex      = P2 0 hmajor
    bl        = P2 (-half_base) (-hminor)

