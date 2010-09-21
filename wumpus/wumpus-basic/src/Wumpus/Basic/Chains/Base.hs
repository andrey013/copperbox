{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Chains.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate points in an iterated chain.
--
-- WARNING - very unstable.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Chains.Base
  (

    AnaAlg(..)
  , IterAlg(..)

  , Chain(..)   -- temporarily open, should be opaque...
  , unchain
  
  , IterSequence
  , iteration
  , bounded
  , pairOnXs
  , pairOnYs

  ) where


import Wumpus.Core                              -- package: wumpus-core


-- | Chains are built as unfolds - AnaAlg avoids the pair 
-- constructor in the usual definition of unfoldr and makes the
-- state strict.
--
-- It is expected that all Chains built on unfolds will terminate. 
--
data AnaAlg st a = Done | Step a !st


-- | IterAlg is a variant of AnaAlg that builds infinite 
-- sequences (iterations).
-- 
-- When lifted to a Chain an iteration is bounded by a count so
-- it will terminate.
--
data IterAlg st a = IterStep a !st 

-- Maybe ux and uy should be exitensially hidden...
--
data Chain ux uy u = forall st. Chain
      { proj_x      :: ux -> u
      , proj_y      :: uy -> u
      , st_zero     :: st
      , gen_step    :: st -> AnaAlg st (ux,uy)
      }

-- Probably there should be another type representing the unfold.
-- Most Chain building functions implemented so far are agnostic 
-- to the scaling functions.

unchain :: Chain ux uy u -> [Point2 u]
unchain (Chain { proj_x = fX, proj_y = fY, st_zero = st0, gen_step = step }) = 
    go $ step st0
  where
    go Done     = []
    go (Step (x,y) next) = P2 (fX x) (fY y) : go (step next)


data IterSequence a = forall st. IterSequence
      { initial_st  :: st
      , iter_step   :: st -> IterAlg st a
      }

iteration :: (a -> a) -> a -> IterSequence a
iteration fn s0 = IterSequence { initial_st = s0, iter_step = step }
  where
    step s = IterStep s (fn s)


bounded :: Int -> IterSequence (ux,uy) -> (ux->u) -> (uy->u) -> Chain ux uy u
bounded n (IterSequence a0 fn) fX fY =
    Chain { proj_x      = fX
          , proj_y      = fY
          , st_zero     = (0,a0)
          , gen_step    = gstep  }
  where
    gstep (i,s) | i < n = let (IterStep ans next) = fn s in Step ans (i+1,next)
    gstep _             = Done




pairOnXs :: (ux -> uy) -> IterSequence ux -> IterSequence (ux,uy)
pairOnXs fn (IterSequence { initial_st = s0, iter_step = step }) = 
    IterSequence s0 step2
  where
    step2 s = let (IterStep a s') = step s in IterStep (a, fn a) s'


pairOnYs :: (r -> l) -> IterSequence r -> IterSequence (l,r) 
pairOnYs fn (IterSequence { initial_st = s0, iter_step = step }) = 
    IterSequence s0 step2
  where
    step2 s = let (IterStep a s') = step s in IterStep (fn a, a) s'

