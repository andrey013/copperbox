{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.InclineTrails
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.InclineTrails
  (

    incline_circle
  , incline_square
  , incline_rect
  , incline_diamond
  , incline_triangle
  , incline_barb
  , incline_tube
  , incline_chamf_rect
  
  )
  where

import Wumpus.Drawing.Basis.DrawingPrimitives

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core


import Data.Monoid

incline_circle :: (Real u, Floating u) => Vec2 u -> AnaTrail u
incline_circle v1 = 
    anaCatTrail zeroVec (semicircleCW v1 <> semicircleCW rv1)
  where
    rv1 = vreverse v1

incline_square :: (Real u, Floating u) => Vec2 u -> AnaTrail u
incline_square v1 = incline_rect (vlength v1) v1


incline_rect :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_rect h v1 = 
    anaCatTrail (theta_down (0.5 * h) ang) catt
  where
    len  = vlength v1
    ang  = vdirection v1
    catt = mconcat [ trail_theta_right len ang
                   , trail_theta_up h ang 
                   , trail_theta_left len ang 
                   , trail_theta_down h ang 
                   ]
           
incline_diamond :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_diamond h v1 = anaCatTrail zeroVec catt
  where
    hw   = 0.5 * vlength v1
    hh   = 0.5 * h
    ang  = vdirection v1
    catt = mconcat [ orthoCatTrail   hw  (-hh) ang
                   , orthoCatTrail   hw    hh  ang 
                   , orthoCatTrail (-hw)   hh  ang 
                   , orthoCatTrail (-hw) (-hh) ang 
                   ]

-- | Note - vector represents midpoint of the baseline to the 
-- tip. Angle is the ang of the tip.
--
-- This trail is primarily for drawing arrowheads.
-- 
incline_triangle :: (Real u, Floating u) => Radian -> Vec2 u -> AnaTrail u
incline_triangle tip_ang v1 = 
    anaCatTrail (theta_up opposite theta) catt
  where
    half_ang = 0.5 * tip_ang
    theta    = vdirection v1
    h        = vlength v1
    opposite = h * (fromRadian $ tan half_ang)

    catt     = mconcat [ trail_theta_adj_grazing h half_ang theta
                       , trail_theta_bkwd_adj_grazing h half_ang theta
                       , trail_theta_up (2 * opposite) theta
                       ]


-- | Note - vector represents midpoint of the baseline to the 
-- tip. Angle is the ang of the tip.
--
-- This trail is primarily for drawing arrowheads. The resulting 
-- path is /open/. 
-- 
incline_barb :: (Real u, Floating u) => Radian -> Vec2 u -> AnaTrail u
incline_barb tip_ang v1 = 
    anaCatTrail (theta_up opposite theta) catt
  where
    half_ang = 0.5 * tip_ang
    theta    = vdirection v1
    h        = vlength v1
    opposite = h * (fromRadian $ tan half_ang)

    catt     = mconcat [ trail_theta_adj_grazing h half_ang theta
                       , trail_theta_bkwd_adj_grazing h half_ang theta
                       ]


-- | @v1@ is the /interior/ vector.
--
incline_tube :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_tube h v1 = 
    anaCatTrail (theta_down_right hh ang) $ mconcat $
      [ trail_theta_right base_len ang
      , semicircleCCW vup
      , trail_theta_left base_len ang
      , semicircleCCW vdown
      ]
  where
    hh        = 0.5 * h
    ang       = vdirection v1 
    base_len  = vlength v1 - h
    vup       = avec (ang + half_pi) h
    vdown     = avec (ang - half_pi) h


incline_chamf_rect :: (Real u, Floating u) => u -> Vec2 u -> AnaTrail u
incline_chamf_rect h v1 = 
    anaCatTrail zeroVec $ mconcat $
      [ trail_theta_down_right hh ang
      , trail_theta_right base_len ang
      , trail_theta_up_right hh ang
      , trail_theta_up_left hh ang
      , trail_theta_left base_len ang
      , trail_theta_down_left hh ang
      ]
  where
    hh        = 0.5 * h
    ang       = vdirection v1 
    base_len  = vlength v1 - h
