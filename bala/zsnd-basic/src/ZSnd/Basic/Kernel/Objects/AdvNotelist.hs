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
          { getAdvNotelist :: TraceLoc ctx u a }

newtype AdvNotelistT ctx u m a = AdvNotelistT 
          { getAdvNotelistT :: TraceLocT ctx u m a }

type instance DUnit (AdvNotelist ctx u a) = u
type instance DUnit (AdvNotelistT ctx u m a) = u

type DAdvNotelist ctx a    = AdvNotelist ctx Double a
type DAdvNotelistT ctx m a = AdvNotelistT ctx Double m a

-- Should staccato be in the original ctx (environment)?




-- Functor 

instance Functor (AdvNotelist ctx u) where
  fmap f mf = AdvNotelist $ fmap f (getAdvNotelist mf)

instance Monad m => Functor (AdvNotelistT ctx u m) where
  fmap f mf = AdvNotelistT $ fmap f (getAdvNotelistT mf)


-- Applicative

instance Applicative (AdvNotelist ctx u) where
  pure a    = AdvNotelist $ pure a
  mf <*> ma = AdvNotelist $ getAdvNotelist mf <*> getAdvNotelist ma

instance Monad m => Applicative (AdvNotelistT ctx u m) where
  pure a    = AdvNotelistT $ pure a
  mf <*> ma = AdvNotelistT $ getAdvNotelistT mf <*> getAdvNotelistT ma


-- Monad

instance Monad (AdvNotelist ctx u) where
  return a  = AdvNotelist $ return a
  ma >>= k  = AdvNotelist $ getAdvNotelist ma >>= \a -> getAdvNotelist (k a)

instance Monad m => Monad (AdvNotelistT ctx u m) where
  return a  = AdvNotelistT $ return a
  ma >>= k  = AdvNotelistT $ getAdvNotelistT ma>>= \a -> getAdvNotelistT (k a)


-- ContextM

instance ContextM (AdvNotelist ctx u) where
  type UCtx (AdvNotelist ctx u) = ctx
  askCtx          = AdvNotelist $ askCtx 
  asksCtx f       = AdvNotelist $ asksCtx f
  localize upd ma = AdvNotelist $ localize upd (getAdvNotelist ma)


instance Monad m => ContextM (AdvNotelistT ctx u m) where
  type UCtx (AdvNotelistT ctx u m) = ctx
  askCtx          = AdvNotelistT $ askCtx
  asksCtx f       = AdvNotelistT $ asksCtx f
  localize upd ma = AdvNotelistT $ localize upd (getAdvNotelistT ma)


-- Monoid

instance Monoid a => Monoid (AdvNotelist ctx u a) where
  mempty           = AdvNotelist $ return mempty
  ma `mappend` mb  = AdvNotelist $ 
                       getAdvNotelist ma `mappend` getAdvNotelist mb


instance (Monad m, Monoid a) => Monoid (AdvNotelistT ctx u m a) where
  mempty           = AdvNotelistT $ return mempty
  ma `mappend` mb  = AdvNotelistT $ 
                       getAdvNotelistT ma `mappend` getAdvNotelistT mb


runAdvNotelist :: InterpretUnit u
               => Context ctx -> AdvNotelist ctx u a -> (a, ULocEvent ctx u)
runAdvNotelist ctx mf = runTraceLoc ctx (getAdvNotelist mf)


execAdvNotelist :: InterpretUnit u
               => Context ctx -> AdvNotelist ctx u a -> ULocEvent ctx u
execAdvNotelist ctx mf = snd $ runAdvNotelist ctx mf

runAdvNotelistT :: (Monad m, InterpretUnit u)
                => Context ctx -> AdvNotelistT ctx u m a -> m (a, ULocEvent ctx u)
runAdvNotelistT ctx mf = runTraceLocT ctx (getAdvNotelistT mf)


execAdvNotelistT :: (Monad m, InterpretUnit u)
                 => Context ctx -> AdvNotelistT ctx u m a -> m (ULocEvent ctx u)
execAdvNotelistT ctx mf = liftM snd $ runAdvNotelistT ctx mf


class AEventM (m :: * -> *) where
  aevent    :: (u ~ DUnit (m ()), ctx ~ UCtx m) 
            => (u -> ULocEvent ctx u) -> u -> m ()

 

-- | @aevent@ scales the playing duration automatically by the 
-- staccato factor and moves the cursor by the full duration.
--

instance (InterpretUnit u, VectorSpace u, Fractional (Scalar u)) 
    => AEventM (AdvNotelist ctx u) where
  aevent mf d = AdvNotelist $ 
                  get_staccato_factor >>= \sd -> 
                  let d1 = d ^* (realToFrac sd)
                  in insertl (mf d1) >> moveBy d


instance ( Monad m, InterpretUnit u, VectorSpace u
         , Fractional (Scalar u)) 
    => AEventM (AdvNotelistT ctx u m) where
  aevent mf d = AdvNotelistT $  
                  get_staccato_factor >>= \sd -> 
                  let d1 = d ^* (realToFrac sd)
                  in insertl (mf d1) >> moveBy d


