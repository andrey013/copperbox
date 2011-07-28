{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.TraceLoc
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Classes for monads that progress (in time) with a cursor.
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.TraceLoc
  (

    TraceLoc
  , TraceLocT
  , DTraceLoc
  , DTraceLocT

  , runTraceLoc
  , evalTraceLoc
  , execTraceLoc

  , runTraceLocT
  , evalTraceLocT
  , execTraceLocT
  , liftTraceLocT
  
  -- * TraceLoc classes
  , TraceLocM(..)
  , LocForkTraceM(..)

  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Control.Monad
import Data.Monoid





newtype TraceLoc itbl ctx u a = TraceLoc
          { getTraceLoc :: u -> (a, u, ULocEvent itbl ctx u) }

newtype TraceLocT itbl ctx u m a = TraceLocT 
          { getTraceLocT :: u -> m (a, u, ULocEvent itbl ctx u) }

type instance DUnit (TraceLoc itbl ctx u a) = u
type instance UCtx  (TraceLoc itbl ctx u)   = ctx
type instance ITbl  (TraceLoc itbl ctx u a) = itbl

type instance DUnit (TraceLocT itbl ctx u m a) = u
type instance UCtx  (TraceLocT itbl ctx u m)   = ctx
type instance ITbl  (TraceLocT itbl ctx u m a) = itbl

type DTraceLoc itbl ctx a    = TraceLoc itbl ctx Double a
type DTraceLocT itbl ctx m a = TraceLocT itbl ctx Double m a



-- Functor 

instance Functor (TraceLoc itbl ctx u) where
  fmap f ma = TraceLoc $ \s0 -> let (a,s1,w) = getTraceLoc ma s0 
                                in (f a, s1, w)


instance Monad m => Functor (TraceLocT itbl ctx u m) where
  fmap f ma = TraceLocT $ \s0 -> getTraceLocT ma s0 >>= \(a,s1,w) -> 
                                 return (f a, s1, w)


-- Applicative

instance Applicative (TraceLoc itbl ctx u) where
  pure a    = TraceLoc $ \s0 -> (a, s0, mempty)
  mf <*> ma = TraceLoc $ \s0 -> 
                let (f,s1,w1) = getTraceLoc mf s0
                    (a,s2,w2) = getTraceLoc ma s1
                in (f a, s2, w1 `mappend` w2)


instance Monad m => Applicative (TraceLocT itbl ctx u m) where
  pure a    = TraceLocT $ \s0 -> return (a, s0, mempty)
  mf <*> ma = TraceLocT $ \s0 -> 
                getTraceLocT mf s0 >>= \(f,s1,w1) ->
                getTraceLocT ma s1 >>= \(a,s2,w2) ->
                return (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (TraceLoc itbl ctx u) where
  return a  = TraceLoc $ \s0 -> (a, s0, mempty)
  ma >>= k  = TraceLoc $ \s0 -> 
                let (a,s1,w1) = getTraceLoc ma s0
                    (b,s2,w2) = (getTraceLoc . k) a s1
                in (b, s2, w1 `mappend` w2)



instance Monad m => Monad (TraceLocT itbl ctx u m) where
  return a  = TraceLocT $ \s0 -> return (a, s0, mempty)
  ma >>= k  = TraceLocT $ \s0 -> 
                getTraceLocT ma s0      >>= \(a,s1,w1) ->
                (getTraceLocT . k) a s1 >>= \(b,s2,w2) -> 
                return (b, s2, w1 `mappend` w2)



-- No instances for ContextM

-- Monoid

instance Monoid a => Monoid (TraceLoc itbl ctx u a) where
  mempty           = TraceLoc $ \s0 -> (mempty, s0, mempty)
  ma `mappend` mb  = TraceLoc $ \s0 -> 
                       let (a,s1,w1) = getTraceLoc ma s0
                           (b,s2,w2) = getTraceLoc mb s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



instance (Monad m, Monoid a) => Monoid (TraceLocT itbl ctx u m a) where
  mempty           = TraceLocT $ \s0 -> return (mempty, s0, mempty)
  ma `mappend` mb  = TraceLocT $ \s0 -> 
                       getTraceLocT ma s0 >>= \(a,s1,w1) ->
                       getTraceLocT mb s1 >>= \(b,s2,w2) ->
                       return (a `mappend` b, s2, w1 `mappend` w2)


runTraceLoc :: Num u 
            => TraceLoc itbl ctx u a -> (a, u, ULocEvent itbl ctx u)
runTraceLoc mf = getTraceLoc mf 0

-- | Forget the generated LocEvent, just return the /answer/.
--
evalTraceLoc :: Num u => TraceLoc itbl ctx u a -> a
evalTraceLoc = post . runTraceLoc
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated ULocEvent.
--
execTraceLoc :: Num u => TraceLoc itbl ctx u a -> ULocEvent itbl ctx u
execTraceLoc = post . runTraceLoc
  where
    post (_,_,o) = o




runTraceLocT :: (Monad m, Num u) 
             => TraceLocT itbl ctx u m a -> m (a, u, ULocEvent itbl ctx u)
runTraceLocT mf = getTraceLocT mf 0


-- | Forget the generated LocImage, just return the /answer/.
--
evalTraceLocT :: (Monad m, Num u) => TraceLocT itbl ctx u m a -> m a
evalTraceLocT = liftM post . runTraceLocT
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated LocImage.
--
execTraceLocT :: (Monad m, Num u) 
              => TraceLocT itbl ctx u m a -> m (ULocEvent itbl ctx u)
execTraceLocT = liftM post . runTraceLocT
  where
    post (_,_,o) = o




liftTraceLocT :: Monad m => m a -> TraceLocT itbl ctx u m a 
liftTraceLocT ma = TraceLocT $ \s0 -> 
                     ma >>= \a -> return (a,s0,mempty)


--------------------------------------------------------------------------------
-- TraceLocM classes

-- | 'insertl' analogue to Writer monad @tell@.
--
class Monad m => TraceLocM (m :: * -> *) where
  insertl   :: (u ~ DUnit (m ()), ctx ~ UCtx m, itbl ~ ITbl (m ())) 
            => ULocEvent itbl ctx u -> m ()
  insertl_  :: (u ~ DUnit (m ()), ctx ~ UCtx m, itbl ~ ITbl (m ())) 
            => LocEvent itbl ctx u a -> m ()
  
  moveBy    :: u ~ DUnit (m ()) => u -> m ()
  location  :: u ~ DUnit (m ()) => m u


  insertl_ = insertl . ignoreAns 



-- Note - @reset@ steals a too general name. 
-- It needs changing...

-- | Add operations for branching (fork at the current point)
-- and resetting to the start point.
-- 
-- Not all drawings that support tracing support branching. For
-- instance Paths can be built by tracing but they always need 
-- a cumulative progression of /next point/ they cannot resrt to 
-- the start point and go in a differnt direction.
-- 
class TraceLocM m => LocForkTraceM (m :: * -> *) where
  reset     :: m ()

  -- Branch is like @local@ in the Reader monad.
  branch    :: m a -> m a




-- TraceLocM

instance InterpretUnit u => TraceLocM (TraceLoc itbl ctx u) where
  insertl gf  = TraceLoc $ \s0 -> ((), s0, moveStart s0 gf)
  moveBy v    = TraceLoc $ \s0 -> ((), s0 + v, mempty)
  location    = TraceLoc $ \s0 -> (s0, s0, mempty)


instance InterpretUnit u => LocForkTraceM (TraceLoc itbl ctx u) where
  reset       = TraceLoc $ \_  -> ((), 0, mempty)
  branch ma   = TraceLoc $ \s0 -> 
                  let (a,_,o) = getTraceLoc ma s0 in (a,s0,o)



instance (Monad m, InterpretUnit u) => 
    TraceLocM (TraceLocT itbl ctx u m) where
  insertl gf  = TraceLocT $ \s0 -> 
                  return ((), s0, moveStart s0 gf)
  moveBy v    = TraceLocT $ \s0 -> return ((), s0 + v, mempty)
  location    = TraceLocT $ \s0 -> return (s0, s0, mempty)

instance (TraceLocM m, InterpretUnit u) => 
    LocForkTraceM (TraceLocT itbl ctx u m) where
  reset       = TraceLocT $ \_  -> return ((), 0, mempty)
  branch ma   = TraceLocT $ \s0 -> getTraceLocT ma s0 >>= \(a,_,o) -> 
                                   return (a,s0,o)

