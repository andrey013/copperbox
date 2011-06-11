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
-- Writer monad with cursor (time).
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.TraceLoc
  (

  -- * TraceLoc monads
    TraceLoc
  , TraceLocT

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

-- | TraceLoc is a writer state monad.
--
-- The writer accumulates a LocGraphic the state is a cumulative
-- displacement vector (called a cursor below).
--
newtype TraceLoc ctx u a = TraceLoc { 
          getTraceLoc :: u -> (a, u, ULocEvent ctx u )}


type instance DUnit (TraceLoc ctx u a) = u
type instance Ctx (TraceLoc ctx u) = ctx


-- | TraceLoc is a writer state monad.
--
-- The writer accumulates a LocGraphic the state is a cumulative
-- displacement vector.
--
newtype TraceLocT ctx u m a = TraceLocT { 
          getTraceLocT :: u -> m (a, u, ULocEvent ctx u )}


type instance DUnit (TraceLocT ctx u m a) = u
type instance Ctx (TraceLocT ctx u m) = ctx




-- Functor

instance Functor (TraceLoc ctx u) where
  fmap f ma = TraceLoc $ \ut -> let (a,u1,o) = getTraceLoc ma ut
                                in (f a, u1, o)

instance Monad m => Functor (TraceLocT ctx u m) where
  fmap f ma = TraceLocT $ \v0 -> getTraceLocT ma v0 >>= \(a,v1,o) -> 
                                 return (f a, v1, o)

-- Applicative

instance Applicative (TraceLoc ctx u) where
  pure a    = TraceLoc $ \v0 -> (a, v0, mempty)
  mf <*> ma = TraceLoc $ \v0 -> 
                let (f,v1,o1) = getTraceLoc mf v0
                    (a,v2,o2) = getTraceLoc ma v1
                in (f a, v2, o1 `mappend` o2)




instance Monad m => Applicative (TraceLocT ctx u m) where
  pure a    = TraceLocT $ \v0 -> return (a, v0, mempty)
  mf <*> ma = TraceLocT $ \v0 -> 
                getTraceLocT mf v0 >>= \(f,v1,o1) ->
                getTraceLocT ma v1 >>= \(a,v2,o2) ->
                return (f a, v2, o1 `mappend` o2)



-- Monad

instance Monad (TraceLoc ctx u) where
  return a  = TraceLoc $ \v0 -> (a, v0, mempty)
  ma >>= k  = TraceLoc $ \v0 -> 
                let (a,v1,o1) = getTraceLoc ma v0
                    (b,v2,o2) = (getTraceLoc . k) a v1
                in (b, v2, o1 `mappend` o2)

instance Monad m => Monad (TraceLocT ctx u m) where
  return a  = TraceLocT $ \v0 -> return (a, v0, mempty)
  ma >>= k  = TraceLocT $ \v0 -> 
                getTraceLocT ma v0 >>= \(a,v1,o1) ->
                (getTraceLocT . k) a v1 >>= \(b,v2,o2) -> 
                return (b, v2, o1 `mappend` o2)



runTraceLoc :: Num u => TraceLoc ctx u a -> (a, u, ULocEvent ctx u)
runTraceLoc mf = getTraceLoc mf 0


-- | Forget the generated LocImage, just return the /answer/.
--
evalTraceLoc :: Num u => TraceLoc ctx u a -> a
evalTraceLoc = post . runTraceLoc
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated LocImage.
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
liftTraceLocT ma = TraceLocT $ \v0 -> 
    ma >>= \a -> return (a,v0,mempty)




-- | 'insertl' analogue to Writer monad @tell@.
--
class Monad m => TraceLocM (m :: * -> *) where
  insertl   :: (u ~ DUnit (m ()), ctx ~ Ctx m) => ULocEvent ctx u -> m ()
  insertl_  :: (u ~ DUnit (m ()), ctx ~ Ctx m) => LocEvent ctx u a -> m ()
  
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




instance (InterpretUnit u, CtxTempo ctx) => 
    TraceLocM (TraceLoc ctx u) where
  insertl gf  = TraceLoc $ \v0 -> ((), v0, moveStart v0 gf)
  moveBy v    = TraceLoc $ \v0 -> ((), v0 + v, mempty)
  location    = TraceLoc $ \v0 -> (v0, v0, mempty)

instance (InterpretUnit u, CtxTempo ctx) => 
    LocForkTraceM (TraceLoc ctx u) where
  reset       = TraceLoc $ \_  -> ((), 0, mempty)
  branch ma   = TraceLoc $ \v0 -> let (a,_,o) = getTraceLoc ma v0 in (a,v0,o)

  

instance (Monad m, InterpretUnit u, CtxTempo ctx) => 
    TraceLocM (TraceLocT ctx u m) where
  insertl gf  = TraceLocT $ \v0 -> return ((), v0, moveStart v0 gf)
  moveBy v    = TraceLocT $ \v0 -> return ((), v0 + v, mempty)
  location    = TraceLocT $ \v0 -> return (v0, v0, mempty)

instance (TraceLocM m, InterpretUnit u, CtxTempo ctx) => 
    LocForkTraceM (TraceLocT ctx u m) where
  reset       = TraceLocT $ \_  -> return ((), 0, mempty)
  branch ma   = TraceLocT $ \v0 -> getTraceLocT ma v0 >>= \(a,_,o) -> 
                                   return (a,v0,o)


