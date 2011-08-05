{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Score.Chain
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- /Chained/ score object (/chain/ is the TikZ drawing object).
--
-- @Where-to-go-next@ is determined by the chain. Individual 
-- events simply /consume/ their position. 
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Score.Chain
  (

    Chain
  , DChain
  , ChainScheme(..)

  , runChain
  , runChain_

  , renderChain
  , renderChainU

  , nextc
  , setNextgen


  , cs_metronomic
  , cs_cycle_beats

  ) where




import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.AdvEvent
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid





newtype Chain ctx u a = Chain
          { getChain :: Context ctx -> OnsetDbl -> ChainSt u 
                     -> (a, OnsetDbl, ChainSt u, CatPrim) }



type instance DUnit (Chain ctx u a) = u
type instance UCtx  (Chain ctx u)   = ctx


type DChain ctx a    = Chain ctx Double a


data ChainScheme st u = ChainScheme 
      { scheme_start    :: st
      , scheme_step     :: u -> st -> (u,st)
      }

type instance DUnit (ChainScheme st u) = u


data ChainSt u = forall st. ChainSt { chain_st :: st, chain_next :: u -> st -> (u,st) }


type instance DUnit (ChainSt u) = u


-- Functor 

instance Functor (Chain ctx u) where
  fmap f ma = Chain $ \r s t -> let (a,s1,t1,w) = getChain ma r s t
                                in (f a, s1, t1, w)



-- Applicative

instance Applicative (Chain ctx u) where
  pure a    = Chain $ \_ s t -> (a, s, t, mempty)
  mf <*> ma = Chain $ \r s t -> 
                let (f,s1,t1,w1) = getChain mf r s t
                    (a,s2,t2,w2) = getChain ma r s1 t1
                in (f a, s2, t2, w1 `mappend` w2)



-- Monad

instance Monad (Chain ctx u) where
  return a  = Chain $ \_ s t -> (a, s, t, mempty)
  ma >>= k  = Chain $ \r s t -> 
                let (a,s1,t1,w1) = getChain ma r s t
                    (b,s2,t2,w2) = (getChain . k) a r s1 t1
                in (b, s2, t2, w1 `mappend` w2)


instance ContextM (Chain ctx u) where
  askCtx          = Chain $ \r s t -> (r, s, t, mempty)
  asksCtx fn      = Chain $ \r s t -> (fn r, s, t, mempty)
  localize upd ma = Chain $ \r s t -> getChain ma (upd r) s t



-- Monoid

instance Monoid a => Monoid (Chain ctx u a) where
  mempty           = Chain $ \_ s t -> (mempty, s, t, mempty)
  ma `mappend` mb  = Chain $ \r s t -> 
                       let (a,s1,t1,w1) = getChain ma r s t
                           (b,s2,t2,w2) = getChain mb r s1 t1
                       in (a `mappend` b, s2, t2, w1 `mappend` w2)


runChain :: InterpretUnit u 
         => ChainScheme st u -> Chain ctx u a -> AdvEvent ctx u a
runChain cscm mf = promoteAdv $ \ot -> 
    askCtx  >>= \ctx ->
    normalizeCtx ot >>= \dot -> 
    let st_zero      = ChainSt { chain_st = scheme_start cscm
                               , chain_next = scheme_step cscm }
        (a, s, _,ca) = getChain mf ctx dot st_zero
        evt          = primEvent ca
        v1           = (dinterp (ctx_tempo ctx) s) - ot
    in replaceAns (a,v1) evt



runChain_ :: InterpretUnit u 
          => ChainScheme st u -> Chain ctx u a -> UAdvEvent ctx u
runChain_ cscm = ignoreAns . runChain cscm


renderChain :: InterpretUnit u 
            => ChainScheme st u -> Context ctx -> Chain ctx u a 
            -> Maybe RScore
renderChain cscm ctx mf = 
   let (_,w1) = runEvent ctx (applyLoc (runAdvEvent $ runChain cscm mf) 0)
   in hprimToScoreMb $ singleH w1


renderChainU :: InterpretUnit u 
             => ChainScheme st u -> Context ctx -> Chain ctx u a -> RScore
renderChainU cscm ctx mf = maybe fk id $ renderChain cscm ctx mf
  where
    fk = error "renderChainU - empty score." 



nextc :: InterpretUnit u 
      => LocEvent ctx u a -> Chain ctx u a
nextc gf  = Chain $ \r s (ChainSt s0 sf) -> 
    let (ot,st1)= sf (dinterp (ctx_tempo r) s) s0
        dot     = normalize (ctx_tempo r) ot
        (a,w1)  = runEvent r (applyLoc gf ot)
    in (a, dot, ChainSt { chain_st = st1, chain_next = sf }, w1)


setNextgen :: ChainScheme st u -> Chain ctx u ()
setNextgen cscm = Chain $ \_ s _ -> 
    let t  = ChainSt { chain_st = scheme_start cscm
                     , chain_next = scheme_step cscm }
    in ((), s, t, mempty) 


--------------------------------------------------------------------------------
-- Predefined schemes


cs_metronomic :: InterpretUnit u => u -> ChainScheme () u
cs_metronomic one_beat = ChainScheme { scheme_start = ()
                                     , scheme_step  = \u a -> (u+one_beat, a) }



cs_cycle_beats :: (InterpretUnit u, VectorSpace u, s ~ Scalar u) 
               => u -> [s] -> ChainScheme [s] u
cs_cycle_beats one_beat xs = ChainScheme { scheme_start = xs
                                         , scheme_step  = fn }
  where
    fn u [s]    = (u + one_beat ^* s, xs)             -- re-seed
    fn u (s:ss) = (u + one_beat ^* s, ss)             -- consume
    fn u []     = (u,[])                              -- should be unreachable
