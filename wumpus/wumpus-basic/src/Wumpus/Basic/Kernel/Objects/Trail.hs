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

  , drawPlacedTrail
  
  , trailIterateLocus

  , diamondTrail
  , polygonTrail

  , sineWaveTrail

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.List ( unfoldr )

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


instance Functor TrailSegment where
  fmap f (TLine v1)        = TLine $ fmap f v1
  fmap f (TCurve v1 v2 v3) = TCurve (fmap f v1) (fmap f v2) (fmap f v3)



drawPlacedTrail :: InterpretUnit u => PathMode -> PlacedTrail u -> LocGraphic u
drawPlacedTrail mode (PlacedTrail v0 xs) = promoteLoc $ \pt -> 
    normalizeCtxF v0 >>= \dv0 -> 
    normalizeCtxF pt >>= \dpt -> 
    liftQuery (mapM (fmap fn . normalizeCtxF) xs) >>= \dxs -> 
    let pp = relPrimPath (dpt .+^ dv0) dxs in dcPath mode pp
  where
    fn (TLine v1)        = relLineTo v1
    fn (TCurve v1 v2 v3) = relCurveTo v1 v2 v3


-- | Create a PlacedTrail from the vector list - each vector in the 
-- input list iterates to the start point rather then the 
-- cumulative tip.
--
-- When the PlacedTrail is run, the supplied point is the /locus/ of 
-- the path and it does not form part of the path proper.
-- 
-- Like 'trailStartIsLocus', this constructor is typically used to 
-- make /shape paths/. Some shapes are easier to express as 
-- iterated displacements of the center rather than 
-- /turtle drawing/. 
-- 
trailIterateLocus :: Num u => [Vec2 u] -> PlacedTrail u
trailIterateLocus []      = PlacedTrail zeroVec []
trailIterateLocus (v0:xs) = PlacedTrail v0 (step v0 xs)
  where
    step v1 []      = [ TLine (v0 ^-^ v1) ]
    step v1 (v2:vs) = TLine (v2 ^-^ v1) : step v2 vs




-- | 'diamondTrail' : @ half_width * half_height -> PlacedTrail @
--
diamondTrail :: Num u => u -> u -> PlacedTrail u
diamondTrail hw hh = trailIterateLocus [ vs,ve,vn,vw ]
  where
    vs = vvec (-hh)
    ve = hvec hw
    vn = vvec hh
    vw = hvec (-hw)


-- | 'polygonTrail' : @ num_points * radius -> PlacedTrail @ 
--
polygonTrail :: Floating u => Int -> u -> PlacedTrail u
polygonTrail n radius = trailIterateLocus $ unfoldr phi (0,top)
  where
    top                     = 0.5*pi
    theta                   = (2*pi) / fromIntegral n
    
    phi (i,ang) | i < n     = Just (avec ang radius, (i+1,ang+theta))
                | otherwise = Nothing

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

