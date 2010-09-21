{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Chain
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

module Wumpus.Basic.Graphic.Chain
  (
    Chain
  , unchain
  
  , IterSequence
  , iteration
  , bounded
  , pairOnXs
  , pairOnYs
  , univariateX
  , univariateY

  , tableDown
  , tableRight

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


data Chain ux uy u = forall st. Chain
      { proj_x      :: ux -> u
      , proj_y      :: uy -> u
      , st_zero     :: st
      , gen_step    :: st -> AnaAlg st (ux,uy)
      }

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


-- Note - should this be a type representing:
--
-- > ScaleX -> ScaleY -> Chain
--
-- After all, the use-case in mind always constructs it as a
-- partial function.
-- 
univariateX :: (Fractional uy, Num ux) 
            => [ux] -> (ux -> u) -> (uy -> u) -> Chain ux uy u
univariateX zs fX fY = 
    Chain { proj_x      = fX
          , proj_y      = fY
          , st_zero     = (0,zs)
          , gen_step    = gstep  }
  where
    gstep (_,[])     = Done 
    gstep (n,x:xs)   = Step (x,n) (n+i,xs)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1


univariateY :: (Fractional ux, Num uy) 
            => [uy] -> (ux -> u) -> (uy -> u) -> Chain ux uy u
univariateY zs fX fY = 
    Chain { proj_x      = fX
          , proj_y      = fY
          , st_zero     = (0,zs)
          , gen_step    = gstep  }
  where
    gstep (_,[])     = Done 
    gstep (n,y:ys)   = Step (n,y) (n+i,ys)
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1



--------------------------------------------------------------------------------
-- Tables


tableDown :: Num u => Int -> Int -> Chain Int Int u 
tableDown rows cols = 
    bounded (rows*cols) (iteration (downstep rows) (0,rows-1)) fromIntegral fromIntegral


downstep :: Int -> (Int,Int) -> (Int,Int)
downstep row_count (x,y) | y == 0 = (x+1,row_count-1)
downstep _         (x,y)          = (x,y-1)



tableRight :: Num u => Int -> Int -> Chain Int Int u 
tableRight rows cols = 
    bounded (rows*cols) (iteration (rightstep cols) (0,rows-1)) fromIntegral fromIntegral


rightstep :: Int -> (Int,Int) -> (Int,Int)
rightstep col_count (x,y) | x == (col_count-1) = (0,y-1)
rightstep _         (x,y)                      = (x+1,y)



--------------------------------------------------------------------------------
-- general helpers

rescale :: Fractional a => a -> a -> a -> a -> a -> a
rescale outmin outmax innmin innmax a = 
    outmin + innpos * (outrange / innrange)  
  where
    outrange = outmax - outmin
    innrange = innmax - innmin
    innpos   = a - innmin 

