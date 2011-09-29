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
           
