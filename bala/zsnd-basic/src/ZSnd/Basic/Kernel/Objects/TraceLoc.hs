{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.TraceLoc
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

module ZSnd.Basic.Kernel.Objects.TraceLoc
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


  )

  where


import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Objects.Basis
import ZSnd.Basic.Kernel.Objects.LocEvent


import Control.Applicative
import Control.Monad
import Data.Monoid





newtype TraceLoc ctx u a = TraceLoc
          { getTraceLoc :: u -> (a, u, ULocEvent ctx u) }

newtype TraceLocT ctx u m a = TraceLocT 
          { getTraceLocT :: u -> m (a, u, ULocEvent ctx u) }

type instance DUnit (TraceLoc ctx u a) = u
type instance UCtx  (TraceLoc ctx u)   = ctx

type instance DUnit (TraceLocT ctx u m a) = u
type instance UCtx  (TraceLocT ctx u m)   = ctx

type DTraceLoc ctx a    = TraceLoc ctx Double a
type DTraceLocT ctx m a = TraceLocT ctx Double m a



-- Functor 

instance Functor (TraceLoc ctx u) where
  fmap f ma = TraceLoc $ \s0 -> let (a,s1,w) = getTraceLoc ma s0 
                                in (f a, s1, w)


instance Monad m => Functor (TraceLocT ctx u m) where
  fmap f ma = TraceLocT $ \s0 -> getTraceLocT ma s0 >>= \(a,s1,w) -> 
                                 return (f a, s1, w)


-- Applicative

instance Applicative (TraceLoc ctx u) where
  pure a    = TraceLoc $ \s0 -> (a, s0, mempty)
  mf <*> ma = TraceLoc $ \s0 -> 
                let (f,s1,w1) = getTraceLoc mf s0
                    (a,s2,w2) = getTraceLoc ma s1
                in (f a, s2, w1 `mappend` w2)


instance Monad m => Applicative (TraceLocT ctx u m) where
  pure a    = TraceLocT $ \s0 -> return (a, s0, mempty)
  mf <*> ma = TraceLocT $ \s0 -> 
                getTraceLocT mf s0 >>= \(f,s1,w1) ->
                getTraceLocT ma s1 >>= \(a,s2,w2) ->
                return (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (TraceLoc ctx u) where
  return a  = TraceLoc $ \s0 -> (a, s0, mempty)
  ma >>= k  = TraceLoc $ \s0 -> 
                let (a,s1,w1) = getTraceLoc ma s0
                    (b,s2,w2) = (getTraceLoc . k) a s1
                in (b, s2, w1 `mappend` w2)



instance Monad m => Monad (TraceLocT ctx u m) where
  return a  = TraceLocT $ \s0 -> return (a, s0, mempty)
  ma >>= k  = TraceLocT $ \s0 -> 
                getTraceLocT ma s0      >>= \(a,s1,w1) ->
                (getTraceLocT . k) a s1 >>= \(b,s2,w2) -> 
                return (b, s2, w1 `mappend` w2)



-- No instances for ContextM

-- Monoid

instance Monoid a => Monoid (TraceLoc ctx u a) where
  mempty           = TraceLoc $ \s0 -> (mempty, s0, mempty)
  ma `mappend` mb  = TraceLoc $ \s0 -> 
                       let (a,s1,w1) = getTraceLoc ma s0
                           (b,s2,w2) = getTraceLoc mb s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



instance (Monad m, Monoid a) => Monoid (TraceLocT ctx u m a) where
  mempty           = TraceLocT $ \s0 -> return (mempty, s0, mempty)
  ma `mappend` mb  = TraceLocT $ \s0 -> 
                       getTraceLocT ma s0 >>= \(a,s1,w1) ->
                       getTraceLocT mb s1 >>= \(b,s2,w2) ->
                       return (a `mappend` b, s2, w1 `mappend` w2)


runTraceLoc :: Num u 
            => TraceLoc ctx u a -> (a, u, ULocEvent ctx u)
runTraceLoc mf = getTraceLoc mf 0

-- | Forget the generated LocEvent, just return the /answer/.
--
evalTraceLoc :: Num u => TraceLoc ctx u a -> a
evalTraceLoc = post . runTraceLoc
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated ULocEvent.
--
execTraceLoc :: Num u => TraceLoc ctx u a -> ULocEvent ctx u
execTraceLoc = post . runTraceLoc
  where
    post (_,_,o) = o




runTraceLocT :: (Monad m, Num u) 
             => TraceLocT ctx u m a -> m (a, u, ULocEvent ctx u)
runTraceLocT mf = getTraceLocT mf 0


-- | Forget the generated LocImage, just return the /answer/.
--
evalTraceLocT :: (Monad m, Num u) => TraceLocT ctx u m a -> m a
evalTraceLocT = liftM post . runTraceLocT
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated LocImage.
--
execTraceLocT :: (Monad m, Num u) => TraceLocT ctx u m a -> m (ULocEvent ctx u)
execTraceLocT = liftM post . runTraceLocT
  where
    post (_,_,o) = o



liftTraceLocT :: Monad m => m a -> TraceLocT ctx u m a 
liftTraceLocT ma = TraceLocT $ \s0 -> 
                     ma >>= \a -> return (a,s0,mempty)


--------------------------------------------------------------------------------
-- TraceLocM classes

-- | 'insertl' analogue to Writer monad @tell@.
--
class Monad m => TraceLocM (m :: * -> *) where
  insertl   :: (u ~ DUnit (m ()), ctx ~ UCtx m) => ULocEvent ctx u -> m ()
  insertl_  :: (u ~ DUnit (m ()), ctx ~ UCtx m) => LocEvent ctx u a -> m ()
  
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

instance InterpretUnit u => TraceLocM (TraceLoc ctx u) where
  insertl gf  = TraceLoc $ \s0 -> ((), s0, moveStart s0 gf)
  moveBy v    = TraceLoc $ \s0 -> ((), s0 + v, mempty)
  location    = TraceLoc $ \s0 -> (s0, s0, mempty)


instance InterpretUnit u => LocForkTraceM (TraceLoc ctx u) where
  reset       = TraceLoc $ \_  -> ((), 0, mempty)
  branch ma   = TraceLoc $ \s0 -> 
                  let (a,_,o) = getTraceLoc ma s0 in (a,s0,o)



instance (Monad m, InterpretUnit u) => 
    TraceLocM (TraceLocT ctx u m) where
  insertl gf  = TraceLocT $ \s0 -> 
                  return ((), s0, moveStart s0 gf)
  moveBy v    = TraceLocT $ \s0 -> return ((), s0 + v, mempty)
  location    = TraceLocT $ \s0 -> return (s0, s0, mempty)

instance (TraceLocM m, InterpretUnit u) => 
    LocForkTraceM (TraceLocT ctx u m) where
  reset       = TraceLocT $ \_  -> return ((), 0, mempty)
  branch ma   = TraceLocT $ \s0 -> getTraceLocT ma s0 >>= \(a,_,o) -> 
                                   return (a,s0,o)

