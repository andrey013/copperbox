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

  , rectangle_trail
  , rrectangle_trail

  )

  where

import Wumpus.Drawing.Basis.InclineTrails

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Data.Monoid


-- Shapes...

circle_trail :: (Real u, Floating u) => u -> AnaTrail u
circle_trail r = rcircle_trail r 0

rcircle_trail :: (Real u, Floating u) => u -> Radian -> AnaTrail u
rcircle_trail r ang = 
    modifyAna (\v -> v ^-^ avec ang r) $ incline_circle $ avec ang (2 * r)


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
