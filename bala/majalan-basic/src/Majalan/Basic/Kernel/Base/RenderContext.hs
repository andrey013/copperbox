{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Base.RenderContext
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Type classes for working with /Context/ (cf. the Reader monad).
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Base.RenderContext
  (
    RenderContext(..)
  , RenderContextM(..)

  , RenderContextF

  , UCtx

  , initialContext

  , normalizeCtx
  , dinterpCtx

  , get_user_context

  , get_tempo
  , set_tempo
  , get_one_beat


  , get_amplitude
  , amplitude
  , scale_amplitude

  , get_staccato_factor
  , staccato_factor

  , get_global_tuning
  , global_tuning

  ) where

import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.DurationUnits

import Control.Applicative

-- | RenderContext
--
-- Note - factors in the context are not universal, some and may 
-- be ignored by a partical Object (notelist, etc.).
-- 
-- @stacccato_factor@ - shortens the sounding of a note, but not 
-- its overall duration.
--
-- @unit_duration@ - duration for instruments that have no dynamic
-- prolongation.
--
data RenderContext uctx = RenderContext
      { ctx_tempo               :: BPM
      , ctx_amplitude           :: Double
      , ctx_staccato_factor     :: Double   -- range 0 .. 1.0
      , ctx_global_tuning       :: Double   -- usually 440.0 (A4)
      , ctx_user_context        :: uctx
      }


type RenderContextF uctx = RenderContext uctx -> RenderContext uctx



type family UCtx m :: *



-- | 'ContextM' is equivalent to the @MonadReader@ class.
--
-- To avoid name clashes with @mtl@ this scheme is used:
--
-- > askCtx   = ask
-- > asksCtx  = asks
-- > localize = local
--

class (Applicative m, Monad m) => RenderContextM (m :: * -> *) where
  askCtx    :: r ~ UCtx m  => m (RenderContext r)
  asksCtx   :: r ~ UCtx m => (RenderContext r -> a) -> m a
  localize  :: r ~ UCtx m => RenderContextF r -> m a -> m a

  asksCtx f  = f <$> askCtx


initialContext :: uctx -> RenderContext uctx
initialContext uc = RenderContext
      { ctx_tempo               = 120
      , ctx_amplitude           = 31662.8
      , ctx_staccato_factor     = 1.0
      , ctx_user_context        = uc
      , ctx_global_tuning       = 440.0
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
-- @RenderContextM@.
--
-- Update: there is a design option to have a parameteric context 
-- though: 
-- 
-- data Ctx uc = Ctx { ctx_tempo :: Tempo, user_ctx :: uc }
--



normalizeCtx :: (RenderContextM m, InterpretUnit u)
             => u -> m Double
normalizeCtx du = (\bpm -> normalize bpm du) <$> get_tempo

dinterpCtx :: (RenderContextM m, InterpretUnit u)
           => Double -> m u
dinterpCtx u = (\bpm -> dinterp bpm u) <$> get_tempo



get_user_context :: (RenderContextM m, uctx ~ UCtx m) => m uctx
get_user_context = asksCtx ctx_user_context


get_tempo :: RenderContextM m => m BPM
get_tempo = asksCtx ctx_tempo

set_tempo :: BPM -> RenderContextF uctx
set_tempo bpm = (\s -> s { ctx_tempo = bpm})

get_one_beat :: (RenderContextM m, InterpretUnit u) => m u
get_one_beat = normalizeCtx (1::Beat) >>= dinterpCtx 


get_amplitude :: RenderContextM m => m Double 
get_amplitude = asksCtx ctx_amplitude


amplitude :: Double -> RenderContextF uctx
amplitude a = (\s -> s { ctx_amplitude = a})

scale_amplitude    :: Double -> RenderContextF uctx
scale_amplitude d  = (\s a -> s { ctx_amplitude = a * d })
                         <*> ctx_amplitude

get_staccato_factor :: RenderContextM m => m Double
get_staccato_factor = asksCtx ctx_staccato_factor


staccato_factor :: Double -> RenderContextF uctx
staccato_factor sd = (\s -> s { ctx_staccato_factor = sd })




get_global_tuning :: RenderContextM m => m Double
get_global_tuning = asksCtx ctx_global_tuning


global_tuning :: Double -> RenderContextF uctx
global_tuning hz = (\s -> s { ctx_global_tuning = hz })
