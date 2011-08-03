{-# LANGUAGE TypeFamilies               #-}
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
-- /Chained/ scored (/chain/ is the TikZ drawing object).
--
-- @Where-to-go-next@ is determined by the chain. Individual 
-- events simply /consume/ their position. 
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Score.Chain
  (

    Chain
  , DChain
  , runChain

  , nextc

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


data ChainSt u = ChainSt { chain_pos :: !OnsetDbl, chain_next :: (u -> u) }


type instance DUnit (ChainSt u) = u


newtype Chain ctx u a = Chain
          { getChain :: Context ctx -> ChainSt u -> (a, ChainSt u, CatPrim) }





type instance DUnit (Chain ctx u a) = u
type instance UCtx  (Chain ctx u)   = ctx


type DChain ctx a    = Chain ctx Double a


-- Functor 

instance Functor (Chain ctx u) where
  fmap f ma = Chain $ \r s -> let (a,s1,w) = getChain ma r s 
                              in (f a, s1, w)



-- Applicative

instance Applicative (Chain ctx u) where
  pure a    = Chain $ \_ s -> (a, s, mempty)
  mf <*> ma = Chain $ \r s -> 
                let (f,s1,w1) = getChain mf r s
                    (a,s2,w2) = getChain ma r s1
                in (f a, s2, w1 `mappend` w2)



-- Monad

instance Monad (Chain ctx u) where
  return a  = Chain $ \_ s -> (a, s, mempty)
  ma >>= k  = Chain $ \r s -> 
                let (a,s1,w1) = getChain ma r s
                    (b,s2,w2) = (getChain . k) a r s1
                in (b, s2, w1 `mappend` w2)


instance ContextM (Chain ctx u) where
  askCtx          = Chain $ \r s -> (r, s, mempty)
  asksCtx fn      = Chain $ \r s -> (fn r, s, mempty)
  localize upd ma = Chain $ \r s -> getChain ma (upd r) s



-- Monoid

instance Monoid a => Monoid (Chain ctx u a) where
  mempty           = Chain $ \_ s -> (mempty, s, mempty)
  ma `mappend` mb  = Chain $ \r s -> 
                       let (a,s1,w1) = getChain ma r s
                           (b,s2,w2) = getChain mb r s1
                       in (a `mappend` b, s2, w1 `mappend` w2)


runChain :: InterpretUnit u => Chain ctx u a -> LocEvent ctx u a
runChain mf = promoteLoc $ \ot -> 
    askCtx  >>= \ctx ->
    normalizeCtx ot >>= \dot -> 
    get_one_beat >>= \size1 -> 
    let next       = \u -> size1 + u   
        st_zero    = ChainSt { chain_pos = dot, chain_next = next }
        (a, _, ca) = getChain mf ctx st_zero
        evt        = primEvent ca
    in replaceAns a $ evt


nextc :: InterpretUnit u => LocEvent ctx u a -> Chain ctx u a
nextc gf  = Chain $ \r s -> 
    let sx         = dinterp (ctx_tempo r) (chain_pos s)
        dx         = normalize (ctx_tempo r) $ chain_next s sx
        PrimW ca a = runEvent r (applyLoc gf sx)
    in (a, s { chain_pos = dx} , ca)

