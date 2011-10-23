{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.ShapeTrails
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.ShapeTrails
  (

    circle_trail
  , rcircle_trail

  , ellipse_trail
  , rellipse_trail

  , rectangle_trail
  , rrectangle_trail

  , diamond_trail
  , rdiamond_trail

  , isosceles_triangle_trail
  , risosceles_triangle_trail

  , semicircle_trail
  , rsemicircle_trail

  , semiellipse_trail
  , rsemiellipse_trail

  , parallelogram_trail
  , rparallelogram_trail

  , trapezium_trail
  , rtrapezium_trail

  )

  where

import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Basis.InclineTrails

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

-- import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Monoid


-- Shapes...

circle_trail :: (Real u, Floating u) => u -> AnaTrail u
circle_trail r = rcircle_trail r 0

rcircle_trail :: (Real u, Floating u) => u -> Radian -> AnaTrail u
rcircle_trail r ang = 
    modifyAna (\v -> v ^-^ avec ang r) $ incline_circle $ avec ang (2 * r)



ellipse_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
ellipse_trail rx ry = rellipse_trail rx ry 0

rellipse_trail :: (Real u, Floating u) => u -> u -> Radian -> AnaTrail u
rellipse_trail rx ry ang = 
    modifyAna (\v -> v ^-^ avec ang rx) $ incline_ellipse ry $ avec ang (2 * rx)


rectangle_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
rectangle_trail w h = rrectangle_trail w h 0


rrectangle_trail :: (Real u, Floating u) => u -> u -> Radian -> AnaTrail u
rrectangle_trail w h ang = 
    anaCatTrail (orthoVec (negate $ 0.5 * w) (negate $ 0.5 * h) ang) catt
  where
    catt = mconcat [ trail_theta_right w ang
                   , trail_theta_up    h ang 
                   , trail_theta_left  w ang 
                   , trail_theta_down  h ang 
                   ]


diamond_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
diamond_trail w h = rdiamond_trail w h 0 

rdiamond_trail :: (Real u, Floating u) => u -> u -> Radian -> AnaTrail u
rdiamond_trail w h ang = 
    anaCatTrail (theta_right hw ang) catt
  where
    hw   = 0.5 * w
    hh   = 0.5 * h
    catt = mconcat [ orthoCatTrail (-hw)   hh  ang
                   , orthoCatTrail (-hw) (-hh) ang 
                   , orthoCatTrail   hw  (-hh) ang 
                   , orthoCatTrail   hw    hh  ang 
                   ]


isosceles_triangle_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
isosceles_triangle_trail bw h = risosceles_triangle_trail bw h 0 

-- | Drawn at the centroid (1/3 * h).
--
risosceles_triangle_trail :: (Real u, Floating u) 
                          => u -> u -> Radian -> AnaTrail u
risosceles_triangle_trail bw h ang = 
    anaCatTrail (orthoVec (negate hbw) (negate $ h / 3) ang) catt
  where
    hbw  = 0.5 * bw
    catt = mconcat [ trail_theta_right bw ang
                   , orthoCatTrail (-hbw)   h  ang
                   , orthoCatTrail (-hbw) (-h) ang
                   ]


semicircle_trail :: (Real u, Floating u) => u -> AnaTrail u
semicircle_trail r = rsemicircle_trail r 0

rsemicircle_trail :: (Real u, Floating u) => u -> Radian -> AnaTrail u
rsemicircle_trail r ang = 
    anaCatTrail (orthoVec (negate r) (negate hminor) ang) catt
  where
    -- hminor is the centroid formula for semicircle
    hminor = (4 * r) / (3 * pi)
    catt   =  trail_theta_right (2 * r) ang
           <> semicircleTrail CCW (avec ang (negate $ 2 * r) )



semiellipse_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
semiellipse_trail rx ry = rsemiellipse_trail rx ry 0

rsemiellipse_trail :: (Real u, Floating u) => u -> u -> Radian -> AnaTrail u
rsemiellipse_trail rx ry ang = 
    anaCatTrail (orthoVec (negate rx) (negate hminor) ang) catt
  where
    -- hminor is the centroid formula for semiellipse
    hminor = (4 * ry) / (3 * pi)
    catt   =  trail_theta_right (2 * rx) ang
           <> semiellipseTrail CCW ry (avec ang (negate $ 2 * rx) )

-- | Note - bottom left angle must be smaller than 180deg, 
-- otherwise a runtime error is thrown.
--
parallelogram_trail :: Floating u => u -> u -> Radian -> AnaTrail u
parallelogram_trail w h bottom_left_ang = 
    rparallelogram_trail w h bottom_left_ang 0

-- | Note - bottom left angle must be smaller than 180deg, 
-- otherwise a runtime error is thrown.
--
rparallelogram_trail :: Floating u => u -> u -> Radian -> Radian -> AnaTrail u
rparallelogram_trail w h bl_ang ang
    | bl_ang >= ang180 = 
        error "rparallelogram_trail - bottom left angle >= 180."
    | otherwise     = anaCatTrail ctr_to_bl catt
  where
    -- Note - base_minor is negative for angles > 90
    base_minor = h / (fromRadian $ tan bl_ang)
                 
    vbase      = theta_right w ang
    vrhs       = orthoVec base_minor h ang
    vtop       = vreverse vbase
    vlhs       = vreverse vrhs
    ctr_to_bl  = vreverse $ 0.5 *^ (vbase ^+^ vrhs)
    catt       = mconcat $ map catline [ vbase, vrhs, vtop, vlhs ]


-- | Note - bottom left angle must be smaller than 180deg, 
-- otherwise a runtime error is thrown.
--
-- Also, no checking is perfomed on the relation between height
-- and bottom_left ang. Out of range values will draw \"twisted\"
-- trapezoids.
-- 
trapezium_trail :: Floating u => u -> u -> Radian -> AnaTrail u
trapezium_trail w h bottom_left_ang = 
    rtrapezium_trail w h bottom_left_ang 0


-- | Note - bottom left angle must be smaller than 180deg, 
-- otherwise a runtime error is thrown.
--
rtrapezium_trail :: Floating u => u -> u -> Radian -> Radian -> AnaTrail u
rtrapezium_trail bw h bl_ang ang  
    | bl_ang >= ang180 = error "rtrapezium_trail - bottom left angle >= 180."
    | otherwise        = anaCatTrail ctr_to_bl catt
  where
    -- Note - base_minor is negative for angles > 90
    base_minor = h / (fromRadian $ tan bl_ang)
    top_width  = bw - (2 * base_minor)
    vbase      = theta_right bw ang
    vrhs       = orthoVec (-base_minor) h ang
    vtop       = theta_left top_width ang
    vlhs       = orthoVec (-base_minor) (-h) ang
    ctr_to_bl  = orthoVec (negate $ 0.5 * bw) (negate $ 0.5 * h) ang
    catt       = mconcat $ map catline [ vbase, vrhs, vtop, vlhs ]

    
