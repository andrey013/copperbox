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
    Context(..)
  , ContextM(..)

  , ContextF

  , initialContext

  , get_tempo
  , set_tempo

  , get_staccato_factor
  , set_staccato_factor

  , normalizeCtx
  , dinterpCtx

  ) where

import ZSnd.Basic.Kernel.Base.BaseDefs

import Control.Applicative


data Context uctx = Context
      { ctx_tempo               :: Tempo
      , ctx_staccato_factor     :: Double   -- range 0 .. 1.0
      , ctx_user_context        :: uctx
      }

-- type family Ctx ctx :: *


type ContextF uctx = Context uctx -> Context uctx

-- | 'ContextM' is equivalent to the @MonadReader@ class.
--
-- To avoid name clashes with @mtl@ this scheme is used:
--
-- > askCtx   = ask
-- > asksCtx  = asks
-- > localize = local
--

class (Applicative m, Monad m) => ContextM (m :: * -> *) where
  type UCtx m :: *
  askCtx    :: r ~ UCtx m  => m (Context r)
  asksCtx   :: r ~ UCtx m => (Context r -> a) -> m a
  localize  :: r ~ UCtx m => ContextF r -> m a -> m a

  asksCtx f  = f <$> askCtx


initialContext :: uctx -> Context uctx
initialContext uc = Context
      { ctx_tempo               = 120
      , ctx_staccato_factor     = 1.0
      , ctx_user_context        = uc
      }



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
-- Update: there is a design option to have a parameteric context 
-- though: 
-- 
-- data Ctx uc = Ctx { ctx_tempo :: Tempo, user_ctx :: uc }
--


get_tempo :: ContextM m => m Tempo
get_tempo = asksCtx ctx_tempo

set_tempo :: Tempo -> ContextF uctx
set_tempo bpm = (\s -> s { ctx_tempo = bpm})



get_staccato_factor :: ContextM m => m Double
get_staccato_factor = asksCtx ctx_staccato_factor


set_staccato_factor :: Double -> ContextF uctx
set_staccato_factor sd = (\s -> s { ctx_staccato_factor = sd })


normalizeCtx :: (ContextM m, InterpretUnit u)
             => u -> m Double
normalizeCtx du = (\bpm -> normalize bpm du) <$> get_tempo

dinterpCtx :: (ContextM m, InterpretUnit u)
           => Double -> m u
dinterpCtx u = (\bpm -> dinterp bpm u) <$> get_tempo

