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

  , Chain
  , chain
  , unchain

  , BivariateAlg
  , bivariate
  
  , SequenceAlg
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
data Chain u = forall ux uy st. Chain
      { proj_x      :: ux -> u
      , proj_y      :: uy -> u
      , bi_alg      :: BivariateAlg ux uy
      }


chain :: (ux -> u) -> (uy -> u) -> BivariateAlg ux uy -> Chain u
chain fx fy alg = Chain { proj_x = fx
                        , proj_y = fy
                        , bi_alg = alg }

unchain :: Chain u -> [Point2 u]
unchain (Chain { proj_x = fx, proj_y = fy, bi_alg = biv}) = 
    runBivariate fx fy biv


-- Probably there should be another type representing the unfold.
-- Most Chain building functions implemented so far are agnostic 
-- to the scaling functions.

data BivariateAlg ux uy = forall st. BivariateAlg
      { st_zero     :: st
      , gen_step    :: st -> AnaAlg st (ux,uy)
      }

bivariate :: st -> (st -> AnaAlg st (ux,uy)) -> BivariateAlg ux uy
bivariate st0 step_alg = BivariateAlg { st_zero = st0
                                      , gen_step = step_alg }


runBivariate :: (ux -> u) -> (uy -> u) -> BivariateAlg ux uy -> [Point2 u]
runBivariate fx fy (BivariateAlg { st_zero = st0, gen_step = step}) = 
    go (step st0)   
  where
    go Done              = []
    go (Step (x,y) next) = P2 (fx x) (fy y) : go (step next)



data SequenceAlg a = forall st. SequenceAlg
      { initial_st  :: st
      , iter_step   :: st -> IterAlg st a
      }

iteration :: (a -> a) -> a -> SequenceAlg a
iteration fn s0 = SequenceAlg { initial_st = s0, iter_step = step }
  where
    step s = IterStep s (fn s)


bounded :: Int -> SequenceAlg (ux,uy) -> BivariateAlg ux uy
bounded n (SequenceAlg a0 fn) =
    BivariateAlg { st_zero     = (0,a0)
                 , gen_step    = gstep  }
  where
    gstep (i,s) | i < n = let (IterStep ans next) = fn s in Step ans (i+1,next)
    gstep _             = Done




pairOnXs :: (ux -> uy) -> SequenceAlg ux -> SequenceAlg (ux,uy)
pairOnXs fn (SequenceAlg { initial_st = s0, iter_step = step }) = 
    SequenceAlg s0 step2
  where
    step2 s = let (IterStep a s') = step s in IterStep (a, fn a) s'


pairOnYs :: (r -> l) -> SequenceAlg r -> SequenceAlg (l,r) 
pairOnYs fn (SequenceAlg { initial_st = s0, iter_step = step }) = 
    SequenceAlg  s0 step2
  where
    step2 s = let (IterStep a s') = step s in IterStep (fn a, a) s'

