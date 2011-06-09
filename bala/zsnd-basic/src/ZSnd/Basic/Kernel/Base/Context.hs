{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Base.Context
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Type classes for working with /Context/ (cf. the Reader monad).
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Base.Context
  (
    Ctx 
  , ContextM(..)

  , CtxTempo(..)
  , get_tempo

  , normalizeCtx
  , dinterpCtx

  ) where

import ZSnd.Basic.Kernel.Base.BaseDefs

import Control.Applicative


type family Ctx ctx :: *


-- | 'ContextM' is equivalent to the @MonadReader@ class.
--
-- To avoid name clashes with @mtl@ this scheme is used:
--
-- > askCtx   = ask
-- > asksCtx  = asks
-- > localize = local
--

class (Applicative m, Monad m) => ContextM (m :: * -> *) where
  askCtx    :: r ~ Ctx m => m r
  asksCtx   :: r ~ Ctx m => (r -> a) -> m a
  localize  :: r ~ Ctx m => (r -> r) -> m a -> m a

  asksCtx f  = f <$> askCtx

--
-- Design Note
--
-- As with Wumpus-Basic, the purpose the /context/ is to avoid 
-- having to fully specify individual instrument statements - most
-- of the parameters are boring and are repeated for each 
-- statement, only certain parameters (e.g. pitch) vary and are
-- /important/ enough to want to be explicit.
-- 
-- Unlike Wumpus, different instruments have very different 
-- parameters so we can\'t have a universal context. The only 
-- useful component this module can provide is the typeclass
-- @ContextM@.
--

class CtxTempo r where
  tempo     :: r -> Tempo
  set_tempo :: Tempo -> r -> r

get_tempo :: (CtxTempo r, ContextM m, r ~ Ctx m)  => m Tempo
get_tempo = asksCtx tempo



normalizeCtx :: (ContextM m, CtxTempo r, InterpretUnit u, r ~ Ctx m)
             => u -> m Double
normalizeCtx du = (\bpm -> normalize bpm du) <$> get_tempo

dinterpCtx :: (ContextM m, CtxTempo r, InterpretUnit u, r ~ Ctx m)
           => Double -> m u
dinterpCtx u = (\bpm -> dinterp bpm u) <$> get_tempo

