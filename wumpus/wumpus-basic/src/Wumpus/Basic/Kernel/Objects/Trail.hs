{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Trial
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Trail
  (

    PlacedTrail(..)
  , GoingTrail(..)
  , TrailSegment(..)

  , sineWaveTrail

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space



data PlacedTrail u = PlacedTrail
      { pt_init_vec :: Vec2 u
      , pt_segments :: [TrailSegment u]
      }
  deriving (Eq,Ord,Show)

data GoingTrail u = GoingTrail 
      { gt_segments :: [TrailSegment u] 
      }
  deriving (Eq,Ord,Show)

type instance DUnit (PlacedTrail u) = u


data TrailSegment u = TLine (Vec2 u)
                    | TCurve (Vec2 u) (Vec2 u) (Vec2 u)
  deriving (Eq,Ord,Show)

type instance DUnit (TrailSegment u) = u




-- | Extend the path with a one-phase sine wave. Height is 
-- parametric.
--
-- > infixl 5 `snocSineWave`
--
sineWaveTrail :: (Real u, Floating u) -- , Ord u, Tolerance u) 
              => u -> Vec2 u -> GoingTrail u
sineWaveTrail h base_vec = GoingTrail $ 
    [ TCurve v1 (vdif v1 v2) (vdif v2 v3)
    , TCurve (vdif v3 v4) (vdif v4 v5) (vdif v5 v6)
    , TCurve (vdif v6 v7) (vdif v7 v8) (vdif v8 v9)
    , TCurve (vdif v9 v10) (vdif v10 v11) (vdif v11 v12)
    ]
  where
    base1 = vlength base_vec / 12
    h2    = h * (pi / 6)
    ang   = vdirection base_vec
    vdif  = flip (^-^)
    v1    = orthoVec     base1    h2  ang
    v2    = orthoVec  (2*base1)   h   ang
    v3    = orthoVec  (3*base1)   h   ang
    v4    = orthoVec  (4*base1)   h   ang
    v5    = orthoVec  (5*base1)   h2  ang
    v6    = orthoVec  (6*base1)   0   ang
    v7    = orthoVec  (7*base1) (-h2) ang
    v8    = orthoVec  (8*base1) (-h)  ang
    v9    = orthoVec  (9*base1) (-h)  ang
    v10   = orthoVec (10*base1) (-h)  ang
    v11   = orthoVec (11*base1) (-h2) ang
    v12   = orthoVec (12*base1)   0   ang

