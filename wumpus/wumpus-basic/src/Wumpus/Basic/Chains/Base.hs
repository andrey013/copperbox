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


    Chain
  , LocChain
  , chain
  , chainFrom
  , unchain


  , AnaAlg(..)
  , IterAlg(..)

  , BivariateAlg
  , bivariate
  
  , SequenceAlg
  , iteration

  , bounded
  , pairOnXs
  , pairOnYs

  ) where

import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core


-- Chain uses the Scaling monad, but it is not itself a monad.


newtype Chain ux uy u = Chain { getChain :: Scaling ux uy u [Point2 u] }


type LocChain ux uy u = Point2 u -> Chain ux uy u



chain :: BivariateAlg ux uy -> Chain ux uy u
chain alg = Chain (scaledBivariatePt alg)

chainFrom :: Num u => BivariateAlg ux uy -> LocChain ux uy u
chainFrom alg start = Chain (scaledBivariateVec alg start)


unchain :: ScalingContext ux uy u -> Chain ux uy u -> [Point2 u]
unchain ctx ch = runScaling ctx $ getChain ch
 

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


data BivariateAlg ux uy = forall st. BivariateAlg
      { st_zero     :: st
      , gen_step    :: st -> AnaAlg st (ux,uy)
      }

bivariate :: st -> (st -> AnaAlg st (ux,uy)) -> BivariateAlg ux uy
bivariate st0 step_alg = BivariateAlg { st_zero = st0
                                      , gen_step = step_alg }


scaledBivariatePt :: BivariateAlg ux uy -> Scaling ux uy u [Point2 u]
scaledBivariatePt (BivariateAlg { st_zero = st0, gen_step = step}) = 
    go (step st0)   
  where
    go Done              = return []
    go (Step (x,y) next) = scalePt x y    >>= \pt   ->
                           go (step next) >>= \rest ->  
                           return  (pt:rest)

-- Note - cannot encode this with (.+^) from Data.AffineSpace.
-- The u (u ~ MonUnit m) extracted from the Scaling Context is 
-- not compatible with the u that forms Points (u ~ Diff (Point2 u)).

scaledBivariateVec :: Num u 
                   => BivariateAlg ux uy 
                   -> Point2 u 
                   -> Scaling ux uy u [Point2 u]
scaledBivariateVec (BivariateAlg { st_zero = st0, gen_step = step}) (P2 x0 y0) = 
    go (step st0)   
  where
    go Done              = return []
    go (Step (x,y) next) = scaleVec x y   >>= \(V2 dx dy)   ->
                           go (step next) >>= \rest ->  
                           return (P2 (x0+dx) (y0+dy):rest)


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

