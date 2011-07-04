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

  , runTraceLocT

  , liftTraceLocT

  -- * TraceLoc classes
  , TraceLocM(..)
  , LocForkTraceM(..)


  )

  where


import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Base.WrappedPrimitive
import ZSnd.Basic.Kernel.Objects.Basis
import ZSnd.Basic.Kernel.Objects.LocEvent


import Control.Applicative
import Control.Monad
import Data.Monoid


type LocCat u = u -> CatPrim

-- | TraceLoc is a writer state monad.
--
-- The writer accumulates a LocGraphic the state is a cumulative
-- displacement vector (called a cursor below).
--
newtype TraceLoc uctx u a = TraceLoc { 
          getTraceLoc :: Context uctx -> u -> (a, u, LocCat u )}


type instance DUnit (TraceLoc ctx u a) = u


-- | TraceLoc is a writer state monad.
--
-- The writer accumulates a LocGraphic the state is a cumulative
-- displacement vector.
--
newtype TraceLocT uctx u m a = TraceLocT { 
          getTraceLocT :: Context uctx -> u -> m (a, u, LocCat u )}


type instance DUnit (TraceLocT ctx u m a) = u




-- Functor

instance Functor (TraceLoc ctx u) where
  fmap f ma = TraceLoc $ \ctx s0 -> let (a,s1,o) = getTraceLoc ma ctx s0 
                                    in (f a, s1, o)

instance Monad m => Functor (TraceLocT ctx u m) where
  fmap f ma = TraceLocT $ \ctx s0 -> getTraceLocT ma ctx s0 >>= \(a,s1,o) -> 
                                     return (f a, s1, o)

-- Applicative

instance Applicative (TraceLoc ctx u) where
  pure a    = TraceLoc $ \_   s0 -> (a, s0, mempty)
  mf <*> ma = TraceLoc $ \ctx s0 -> 
                let (f,s1,o1) = getTraceLoc mf ctx s0
                    (a,s2,o2) = getTraceLoc ma ctx s1
                in (f a, s2, o1 `mappend` o2)


instance Monad m => Applicative (TraceLocT ctx u m) where
  pure a    = TraceLocT $ \_   s0 -> return (a, s0, mempty)
  mf <*> ma = TraceLocT $ \ctx s0 -> 
                getTraceLocT mf ctx s0 >>= \(f,s1,o1) ->
                getTraceLocT ma ctx s1 >>= \(a,s2,o2) ->
                return (f a, s2, o1 `mappend` o2)



-- Monad

instance Monad (TraceLoc ctx u) where
  return a  = TraceLoc $ \_   s0 -> (a, s0, mempty)
  ma >>= k  = TraceLoc $ \ctx s0 -> 
                let (a,s1,o1) = getTraceLoc ma ctx s0
                    (b,s2,o2) = (getTraceLoc . k) a ctx s1
                in (b, s2, o1 `mappend` o2)

instance Monad m => Monad (TraceLocT ctx u m) where
  return a  = TraceLocT $ \_   s0 -> return (a, s0, mempty)
  ma >>= k  = TraceLocT $ \ctx s0 -> 
                getTraceLocT ma ctx s0      >>= \(a,s1,o1) ->
                (getTraceLocT . k) a ctx s1 >>= \(b,s2,o2) -> 
                return (b, s2, o1 `mappend` o2)


-- ContextM

-- The ContextM instance mandates the /function splitting/ of the 
-- accumulator. But it seems import to have a ContextM instance
-- to match TraceNotelist.

instance ContextM (TraceLoc ctx u) where
  type UCtx (TraceLoc ctx u) = ctx
  askCtx          = TraceLoc $ \ctx s0 -> (ctx, s0, mempty)
  asksCtx f       = TraceLoc $ \ctx s0 -> (f ctx, s0, mempty)
  localize upd ma = TraceLoc $ \ctx s0 -> getTraceLoc ma (upd ctx) s0

instance Monad m => ContextM (TraceLocT ctx u m) where
  type UCtx (TraceLocT ctx u m) = ctx
  askCtx          = TraceLocT $ \ctx s0 -> return (ctx, s0, mempty)
  asksCtx f       = TraceLocT $ \ctx s0 -> return (f ctx, s0, mempty)
  localize upd ma = TraceLocT $ \ctx s0 -> getTraceLocT ma (upd ctx) s0


-- Monoid

instance Monoid a => Monoid (TraceLoc ctx u a) where
  mempty          = TraceLoc $ \_   s0 -> (mempty, s0, mempty)
  ma `mappend` mb = TraceLoc $ \ctx s0 -> 
                      let (a,s1,w1) = getTraceLoc ma ctx s0
                          (b,s2,w2) = getTraceLoc mb ctx s1
                      in (a `mappend` b, s2, w1 `mappend` w2)

instance (Monad m, Monoid a) => Monoid (TraceLocT ctx u m a) where
  mempty          = TraceLocT $ \_   s0 -> return (mempty, s0, mempty)
  ma `mappend` mb = TraceLocT $ \ctx s0 -> 
                      getTraceLocT ma ctx s0 >>= \(a,s1,w1) ->
                      getTraceLocT mb ctx s1 >>= \(b,s2,w2) ->
                      return (a `mappend` b, s2, w1 `mappend` w2)




runTraceLoc :: InterpretUnit u
            => Context ctx -> TraceLoc ctx u a -> (a,ULocEvent ctx u)
runTraceLoc ctx mf = 
    let (a,_,df) = getTraceLoc mf ctx 0
    in (a, promoteLoc $ \ot -> primEvent (df ot))


runTraceLocT :: (Monad m, InterpretUnit u) 
             => Context ctx -> TraceLocT ctx u m a -> m (a, ULocEvent ctx u)
runTraceLocT ctx mf = getTraceLocT mf ctx 0 >>= \(a,_,df) -> 
                      return (a, promoteLoc $ \ot -> primEvent (df ot))



liftTraceLocT :: Monad m => m a -> TraceLocT ctx u m a 
liftTraceLocT ma = TraceLocT $ \_ v0 -> 
    ma >>= \a -> return (a,v0,mempty)




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




instance InterpretUnit u => TraceLocM (TraceLoc ctx u) where
  insertl gf  = TraceLoc $ \ctx s0 -> 
                   let fn = \u -> case runLocEvent u ctx $ moveStart s0 gf of
                                    PrimW cp _ -> cp
                   in ((), s0, fn)
  moveBy v    = TraceLoc $ \_   s0 -> ((), s0 + v, mempty)
  location    = TraceLoc $ \_   s0 -> (s0, s0, mempty)

instance InterpretUnit u => LocForkTraceM (TraceLoc ctx u) where
  reset       = TraceLoc $ \_   _  -> ((), 0, mempty)
  branch ma   = TraceLoc $ \ctx s0 -> 
                  let (a,_,o) = getTraceLoc ma ctx s0 in (a,s0,o)

  

instance (Monad m, InterpretUnit u) => 
    TraceLocM (TraceLocT ctx u m) where
  insertl gf  = TraceLocT $ \ctx v0 -> 
                   let fn = \u -> case runLocEvent u ctx $ moveStart v0 gf of
                                    PrimW cp _ -> cp
                   in return ((), v0, fn)
  moveBy v    = TraceLocT $ \_   v0 -> return ((), v0 + v, mempty)
  location    = TraceLocT $ \_   v0 -> return (v0, v0, mempty)

instance (TraceLocM m, InterpretUnit u) => 
    LocForkTraceM (TraceLocT ctx u m) where
  reset       = TraceLocT $ \_   _  -> return ((), 0, mempty)
  branch ma   = TraceLocT $ \ctx v0 -> getTraceLocT ma ctx v0 >>= \(a,_,o) -> 
                                       return (a,v0,o)


