{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.AdvNotelist
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ...
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.AdvNotelist
  ( 

   
  -- * Note list
    AdvNotelist
  , DAdvNotelist
  , AdvNotelistT
  , DAdvNotelistT


  , runAdvNotelist
  , execAdvNotelist

  , runAdvNotelistT
  , execAdvNotelistT

  , AEventM(..)

  ) where

import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Objects.LocEvent
import ZSnd.Basic.Kernel.Objects.TraceLoc

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid



newtype AdvNotelist ctx u a = AdvNotelist 
          { getAdvNotelist :: Env -> TraceLoc ctx u a }

newtype AdvNotelistT ctx u m a = AdvNotelistT 
          { getAdvNotelistT :: Env -> TraceLocT ctx u m a }

type instance DUnit (AdvNotelist ctx u a) = u
type instance DUnit (AdvNotelistT ctx u m a) = u

type instance Ctx (AdvNotelist ctx u) = ctx
type instance Ctx (AdvNotelistT ctx u m) = ctx

type DAdvNotelist ctx a    = AdvNotelist ctx Double a
type DAdvNotelistT ctx m a = AdvNotelistT ctx Double m a

-- Should staccato be in the original ctx (environment)?

data Env = Env
      { staccato_factor :: Double       -- should be [0.. 1.0]
      } 

envZero :: Env
envZero = Env { staccato_factor = 1.0 }



-- Functor 

instance Functor (AdvNotelist ctx u) where
  fmap f mf = AdvNotelist $ \r -> fmap f (getAdvNotelist mf r)

instance Monad m => Functor (AdvNotelistT ctx u m) where
  fmap f mf = AdvNotelistT $ \r -> fmap f (getAdvNotelistT mf r)


-- Applicative

instance Applicative (AdvNotelist ctx u) where
  pure a    = AdvNotelist $ \_ -> pure a
  mf <*> ma = AdvNotelist $ \r -> getAdvNotelist mf r <*> getAdvNotelist ma r

instance Monad m => Applicative (AdvNotelistT ctx u m) where
  pure a    = AdvNotelistT $ \_ -> pure a
  mf <*> ma = AdvNotelistT $ \r -> getAdvNotelistT mf r <*> getAdvNotelistT ma r


-- Monad

instance Monad (AdvNotelist ctx u) where
  return a  = AdvNotelist $ \_ -> return a
  ma >>= k  = AdvNotelist $ \r -> getAdvNotelist ma r >>= \a -> 
                                  getAdvNotelist (k a) r

instance Monad m => Monad (AdvNotelistT ctx u m) where
  return a  = AdvNotelistT $ \_ -> return a
  ma >>= k  = AdvNotelistT $ \r -> getAdvNotelistT ma r >>= \a -> 
                                   getAdvNotelistT (k a) r


-- ContextM

instance ContextM (AdvNotelist ctx u) where
  askCtx          = AdvNotelist $ \_ -> askCtx 
  asksCtx f       = AdvNotelist $ \_ -> asksCtx f
  localize upd ma = AdvNotelist $ \r -> localize upd (getAdvNotelist ma r)


instance Monad m => ContextM (AdvNotelistT ctx u m) where
  askCtx          = AdvNotelistT $ \_ -> askCtx
  asksCtx f       = AdvNotelistT $ \_ -> asksCtx f
  localize upd ma = AdvNotelistT $ \r -> localize upd (getAdvNotelistT ma r)


-- Monoid

instance Monoid a => Monoid (AdvNotelist ctx u a) where
  mempty           = AdvNotelist $ \_ -> return mempty
  ma `mappend` mb  = AdvNotelist $ \r -> 
                       getAdvNotelist ma r `mappend` getAdvNotelist mb r


instance (Monad m, Monoid a) => Monoid (AdvNotelistT ctx u m a) where
  mempty           = AdvNotelistT $ \_ -> return mempty
  ma `mappend` mb  = AdvNotelistT $ \r -> 
                       getAdvNotelistT ma r `mappend` getAdvNotelistT mb r


runAdvNotelist :: (CtxTempo ctx, InterpretUnit u)
               => ctx -> AdvNotelist ctx u a -> (a, ULocEvent ctx u)
runAdvNotelist ctx mf = runTraceLoc ctx (getAdvNotelist mf envZero)


execAdvNotelist :: (CtxTempo ctx, InterpretUnit u)
               => ctx -> AdvNotelist ctx u a -> ULocEvent ctx u
execAdvNotelist ctx mf = snd $ runAdvNotelist ctx mf

runAdvNotelistT :: (Monad m, CtxTempo ctx, InterpretUnit u)
                => ctx -> AdvNotelistT ctx u m a -> m (a, ULocEvent ctx u)
runAdvNotelistT ctx mf = runTraceLocT ctx (getAdvNotelistT mf envZero)


execAdvNotelistT :: (Monad m, CtxTempo ctx, InterpretUnit u)
                 => ctx -> AdvNotelistT ctx u m a -> m (ULocEvent ctx u)
execAdvNotelistT ctx mf = liftM snd $ runAdvNotelistT ctx mf


class AEventM (m :: * -> *) where
  aevent    :: (u ~ DUnit (m ()), ctx ~ Ctx m) 
            => (u -> ULocEvent ctx u) -> u -> m ()

  staccato  :: Double -> m a -> m a
 

-- | @aevent@ scales the playing duration automatically by the 
-- staccato factor and moves the cursor by the full duration.
--

instance (CtxTempo ctx, InterpretUnit u, VectorSpace u, Fractional (Scalar u)) 
    => AEventM (AdvNotelist ctx u) where
  aevent mf d = AdvNotelist $ \r -> 
                  let d1 = d ^* (realToFrac $ staccato_factor r)
                  in insertl (mf d1) >> moveBy d

  staccato d ma = AdvNotelist $ \r -> 
                    getAdvNotelist ma (r { staccato_factor = d})


instance ( Monad m, CtxTempo ctx, InterpretUnit u, VectorSpace u
         , Fractional (Scalar u)) 
    => AEventM (AdvNotelistT ctx u m) where
  aevent mf d = AdvNotelistT $ \r -> 
                  let d1 = d ^* (realToFrac $ staccato_factor r)
                  in insertl (mf d1) >> moveBy d

  staccato d ma = AdvNotelistT $ \r -> 
                    getAdvNotelistT ma (r { staccato_factor = d})

