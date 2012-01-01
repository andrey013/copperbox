{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.Chain
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Chaining events.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.Chain
  ( 
   
    ChainScheme(..)
  , GenChain
  , Chain

  , runGenChain
  , evalGenChain
  , execGenChain
  , runChain
  , runChain_

  , chain1
  , chainSkip_
  , chainMany
  , chainReplicate
  , chainCount

  -- schemes
  , iterationScheme
  , sequenceScheme
  , countingScheme

  ) where


import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.RenderContext
import ZMidi.Basic.Kernel.Base.WrappedPrimitive
import ZMidi.Basic.Kernel.Objects.Event

import Control.Applicative
import Data.Monoid

-- | scheme_start is a function from the origin to state.
-- 
-- For instance, we might want to cache the origin - this would
-- not be possible if start was just a pure @cst@ value. 
--
data ChainScheme u = forall cst. ChainScheme 
      { chain_init      :: u -> cst
      , chain_step      :: u -> cst -> (u,cst)
      }


newtype GenChain st u a = GenChain
          { getGenChain :: RenderContext -> OnsetTime 
                        -> DTimeSpan -> ChainSt st u 
                        -> (a, OnsetTime, DTimeSpan, ChainSt st u, CatPrim) }


type Chain u a = GenChain () u a

data ChainSt st u = forall cst. ChainSt 
       { chain_count      :: Int
       , chain_st         :: cst
       , chain_next       :: u -> cst -> (u,cst) 
       , chain_user_state :: st
       }


-- Functor 

instance Functor (GenChain st u) where
  fmap f ma = GenChain $ \ctx loc ts s -> 
                let (a,loc1,ts1,s1,w1) = getGenChain ma ctx loc ts s 
                in (f a, loc1, ts1, s1, w1)


-- Applicative

instance Applicative (GenChain st u) where
  pure a    = GenChain $ \_   loc ts s -> (a, loc, ts, s, mempty)
  mf <*> ma = GenChain $ \ctx loc ts s -> 
                let (f,loc1,ts1,s1,w1) = getGenChain mf ctx loc ts s
                    (a,loc2,ts2,s2,w2) = getGenChain ma ctx loc1 ts1 s1
                in (f a, loc2, ts2, s2, w1 `mappend` w2)



-- Monad

instance Monad (GenChain st u) where
  return a  = GenChain $ \_   loc ts s -> (a, loc, ts, s, mempty)
  ma >>= k  = GenChain $ \ctx loc ts s -> 
                let (a,loc1,ts1,s1,w1) = getGenChain ma ctx loc ts s
                    (b,loc2,ts2,s2,w2) = (getGenChain . k) a ctx loc1 ts1 s1
                in (b, loc2, ts2, s2, w1 `mappend` w2)


-- Monoid

instance Monoid a => Monoid (GenChain st u a) where
  mempty           = GenChain $ \_   loc ts s -> (mempty, loc, ts, s, mempty)
  ma `mappend` mb  = GenChain $ \ctx loc ts s -> 
                       let (a,loc1,ts1,s1,w1) = getGenChain ma ctx loc ts s
                           (b,loc2,ts2,s2,w2) = getGenChain mb ctx loc1 ts1 s1
                       in (a `mappend` b, loc2, ts2, s2, w1 `mappend` w2)


-- RenderContextM

instance RenderContextM (GenChain st u) where
  askCtx          = GenChain $ \ctx loc ts s -> (ctx, loc, ts, s, mempty)
  asksCtx fn      = GenChain $ \ctx loc ts s -> (fn ctx, loc, ts, s, mempty)
  localize upd ma = GenChain $ \ctx loc ts s -> getGenChain ma (upd ctx) loc ts s


type instance UState (GenChain st u) = st

-- UserStateM 

instance UserStateM (GenChain st u) where
  getState        = GenChain $ \_ loc ts s@(ChainSt _ _ _ ust) -> 
                      (ust, loc, ts, s, mempty)

  setState ust    = GenChain $ \_ loc ts (ChainSt i a b _) -> 
                      ((), loc, ts, ChainSt i a b ust, mempty)

  updateState upd = GenChain $ \_ loc ts (ChainSt i a b ust) -> 
                      ((), loc, ts, ChainSt i a b (upd ust), mempty)


--------------------------------------------------------------------------------
-- Run functions



runGenChain :: InterpretUnit u 
            => ChainScheme u -> st -> GenChain st u a -> Event u (a,st)
runGenChain (ChainScheme start step) ust ma = promoteLoc $ \ctx loc -> 
    let st_zero         = ChainSt { chain_count      = 0
                                  , chain_st         = start loc
                                  , chain_next       = step
                                  , chain_user_state = ust }
        bpm             = interp_bpm ctx
        dloc            = normalize bpm loc
        ts0             = instantTimeSpan dloc
        (a,_,ts,s1,w1)  = getGenChain ma ctx dloc ts0 st_zero
        uts             = dinterpTimeSpan bpm ts
    in ((a, chain_user_state s1),uts,w1)

-- | Forget the user state LocImage, just return the /answer/.
--
evalGenChain :: InterpretUnit u 
             => ChainScheme u -> st -> GenChain st u a -> Event u a
evalGenChain cscm st ma = fmap fst $ runGenChain cscm st ma


-- | Forget the /answer/, just return the user state.
--
execGenChain :: InterpretUnit u 
             => ChainScheme u -> st -> GenChain st u a -> Event u st 
execGenChain cscm st ma = fmap snd $ runGenChain cscm st ma

runChain :: InterpretUnit u 
         => ChainScheme u -> Chain u a -> Event u a
runChain cscm ma = evalGenChain cscm () ma

runChain_ :: InterpretUnit u 
          => ChainScheme u -> Chain u a -> Event u ()
runChain_ cscm ma = ignoreAns $ runChain cscm ma



-- | Demand a location on the Chain and run the Event at it.
--
chain1 :: InterpretUnit u 
       => Event u a -> GenChain st u a
chain1 gf  = GenChain $ \ctx loc ts (ChainSt i0 s0 sf ust) -> 
    let bpm         = interp_bpm ctx
        uloc        = dinterp bpm loc
        (a,ts1,w1)  = runEvent ctx uloc gf
        dts1        = normalizeTimeSpan bpm ts1
        (loc1,st1)  = sf uloc s0
        dloc1       = normalize bpm loc1
        new_st      = ChainSt { chain_count      = i0 + 1
                              , chain_st         = st1
                              , chain_next       = sf
                              , chain_user_state = ust }
    in (a, dloc1, unionTimeSpan ts dts1, new_st, w1)



-- | Demand the next position, but generate no event.
-- 
--
chainSkip_ :: InterpretUnit u => GenChain st u ()
chainSkip_ = chain1 (blank 0) 



-- | Chain a list of events, each demanding succesives start 
-- points.
--
chainMany :: InterpretUnit u 
          => [Event u a] -> GenChain st u ()
chainMany = mapM_ (chain1 . ignoreAns)



-- | Replicate a Event @n@ times along a Chain.
--
chainReplicate :: InterpretUnit u 
               => Int -> Event u a -> GenChain st u ()
chainReplicate n = chainMany . replicate n 


-- | Return the count of chain steps.
--
chainCount :: GenChain st u Int
chainCount = GenChain $ \_ dloc ts st@(ChainSt i _ _ _) -> (i, dloc, ts, st, mempty)



--------------------------------------------------------------------------------
-- Schemes


-- | General scheme - iterate the next location with the supplied
-- function.
--
iterationScheme :: (u -> u) -> ChainScheme u
iterationScheme fn = ChainScheme { chain_init = const ()
                                 , chain_step = \loc _ -> (fn loc, ())
                                 }


-- | General scheme - displace successively by the elements of the
-- list of vectors. 
-- 
-- Note - the list is cycled to make the chain infinite.
--
sequenceScheme :: Num u => [u] -> ChainScheme u
sequenceScheme [] = error "sequenceScheme - empty list."
sequenceScheme vs = ChainScheme { chain_init = const $ cycle vs
                                , chain_step = step
                                }
  where
    step _   []     = error "sequenceScheme - unreachable, cycled."
    step loc (w:ws) = (loc + w, ws) 



-- | Build an (infinite) ChainScheme for a prefix list of counted 
-- schemes and a final scheme that runs out to infinity.
--
countingScheme :: [(Int, ChainScheme u)] -> ChainScheme u -> ChainScheme u
countingScheme []     rest = rest
countingScheme (x:xs) rest = chainPrefix  x (countingScheme xs rest)


-- | Helper - complicated...
--
chainPrefix :: (Int, ChainScheme u) -> ChainScheme u -> ChainScheme u
chainPrefix (ntimes, ChainScheme astart astep) rest@(ChainScheme bstart bstep)
    | ntimes < 1 = rest
    | otherwise  = ChainScheme { chain_init = start, chain_step = next }
  where
    start pt = (astart pt,ntimes, bstart pt)

    next pt (ast,n,bst) 
        | n > 0     = let (p2,ast1) = astep pt ast in (p2, (ast1,n-1,bst))
        | n == 0    = let bst1      = bstart pt 
                          (p2,bst2) = bstep pt bst1 
                      in (p2, (ast,(-1),bst2))
        | otherwise = let (p2,bst1) = bstep pt bst in (p2,(ast, (-1), bst1))