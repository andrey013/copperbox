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
  , ChainScheme

  , runChain
  , runChain_

  , renderChain
  , renderChainU

  , nextc
  , setNextgen

  ) where




import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Control.Monad
import Data.Monoid





newtype Chain st ctx u a = Chain
          { getChain :: Context ctx -> OnsetDbl -> ChainSt st u 
                     -> (a, OnsetDbl, ChainSt st u, CatPrim) }



type instance DUnit (Chain st ctx u a) = u
type instance UCtx  (Chain st ctx u)   = ctx


type DChain st ctx a    = Chain st ctx Double a


data ChainScheme st u = ChainScheme 
      { _scheme_start    :: u -> st
      , _scheme_step     :: u -> st -> (u,st)
      }

type instance DUnit (ChainScheme st u) = u


data ChainSt st u = ChainSt { chain_st :: st, chain_next :: u -> st -> (u,st) }


type instance DUnit (ChainSt st u) = u


-- Functor 

instance Functor (Chain st ctx u) where
  fmap f ma = Chain $ \r s t -> let (a,s1,t1,w) = getChain ma r s t
                                in (f a, s1, t1, w)



-- Applicative

instance Applicative (Chain st ctx u) where
  pure a    = Chain $ \_ s t -> (a, s, t, mempty)
  mf <*> ma = Chain $ \r s t -> 
                let (f,s1,t1,w1) = getChain mf r s t
                    (a,s2,t2,w2) = getChain ma r s1 t1
                in (f a, s2, t2, w1 `mappend` w2)



-- Monad

instance Monad (Chain st ctx u) where
  return a  = Chain $ \_ s t -> (a, s, t, mempty)
  ma >>= k  = Chain $ \r s t -> 
                let (a,s1,t1,w1) = getChain ma r s t
                    (b,s2,t2,w2) = (getChain . k) a r s1 t1
                in (b, s2, t2, w1 `mappend` w2)


instance ContextM (Chain st ctx u) where
  askCtx          = Chain $ \r s t -> (r, s, t, mempty)
  asksCtx fn      = Chain $ \r s t -> (fn r, s, t, mempty)
  localize upd ma = Chain $ \r s t -> getChain ma (upd r) s t



-- Monoid

instance Monoid a => Monoid (Chain st ctx u a) where
  mempty           = Chain $ \_ s t -> (mempty, s, t, mempty)
  ma `mappend` mb  = Chain $ \r s t -> 
                       let (a,s1,t1,w1) = getChain ma r s t
                           (b,s2,t2,w2) = getChain mb r s1 t1
                       in (a `mappend` b, s2, t2, w1 `mappend` w2)


runChain :: InterpretUnit u 
         => ChainScheme st u -> Chain st ctx u a -> LocEvent ctx u a
runChain (ChainScheme csStart csNext) mf = promoteLoc $ \ot -> 
    askCtx  >>= \ctx ->
    normalizeCtx ot >>= \dot -> 
    let st_zero      = ChainSt { chain_st = csStart ot, chain_next = csNext }
        (a, _, _,ca) = getChain mf ctx dot st_zero
        evt          = primEvent ca
    in replaceAns a $ evt


runChain_ :: InterpretUnit u 
          => ChainScheme st u -> Chain st ctx u a -> ULocEvent ctx u
runChain_ cscm = ignoreAns . runChain cscm

renderChain :: InterpretUnit u 
            => ChainScheme st u -> Context ctx -> Chain st ctx u a 
            -> Maybe RScore
renderChain cscm ctx mf = 
   let PrimW ca _ = runEvent ctx (applyLoc (runChain cscm mf) 0)
   in hprimToScoreMb $ singleH ca


renderChainU :: InterpretUnit u 
             => ChainScheme st u -> Context ctx -> Chain st ctx u a -> RScore
renderChainU cscm ctx mf = maybe fk id $ renderChain cscm ctx mf
  where
    fk = error "renderChainU - empty score." 



nextc :: InterpretUnit u 
      => LocEvent ctx u a -> Chain st ctx u a
nextc gf  = Chain $ \r s t@(ChainSt s0 sf) -> 
    let (ot,st1)   = sf (dinterp (ctx_tempo r) s) s0
        dot        = normalize (ctx_tempo r) ot
        PrimW ca a = runEvent r (applyLoc gf ot)
    in (a, dot, t { chain_st = st1}, ca)


setNextgen :: InterpretUnit u 
           => ChainScheme st u -> Chain st ctx u ()
setNextgen (ChainScheme cStart cNext) = Chain $ \r s _ -> 
    let ot = dinterp (ctx_tempo r) s
        t  = ChainSt { chain_st = cStart ot, chain_next = cNext}
    in ((), s, t, mempty) 

