{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Base.RenderContext
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Base.RenderContext
  ( 

  -- * RenderContext type
    RenderContext(..)
  , RenderContextF

  , standardContext
  , default_volume
  , default_pan

  -- * RenderCtxM (reader) monad class
  , RenderCtxM(..)


  -- * Queries
  , getVolume

  -- * Updates
  , set_volume

  ) where

import Control.Applicative
import Data.Word

data RenderContext = RenderContext 
      { rc_volume               :: !Word16
      , rc_pan                  :: !Word16 
      , rc_velo_note_on         :: !Word8
      , rc_velo_note_off        :: !Word8
      } 


type RenderContextF = RenderContext -> RenderContext


standardContext :: RenderContext
standardContext = 
    RenderContext { rc_volume               = default_volume
                  , rc_pan                  = default_pan
                  , rc_velo_note_on         = 127
                  , rc_velo_note_off        = 63
                  } 


default_volume          :: Word16
default_volume          = 127


default_pan             :: Word16
default_pan             = 0x7F7F `div` 2



-- | 'RenderCtxM' is equivalent to the to the @MonadReader@ 
-- class, but the environment type is fixed to 'RenderContext'.
--
-- To avoid name clashes with @mtl@ this scheme is used:
--
-- > askDC    = ask
-- > asksDC   = asks
-- > localize = local
--
-- Note, because the derived operation 'query' (aka @asks@) is
-- expected to be used more often than queryCtx (aka @ask@) it 
-- gets the more convenient name.
--
class (Applicative m, Monad m) => RenderCtxM (m :: * -> *) where
  askCtx    :: m RenderContext
  asksCtx   :: (RenderContext -> a) -> m a
  localize  :: (RenderContext -> RenderContext) -> m a -> m a

  asksCtx f  = f <$> askCtx



--------------------------------------------------------------------------------
-- Queries

getVolume :: RenderCtxM m => m Word16
getVolume = asksCtx rc_volume


--------------------------------------------------------------------------------
-- Updates

set_volume :: Word16 -> RenderContextF
set_volume i = (\s -> s { rc_volume = i })