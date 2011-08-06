{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.LocTrace
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Writer monad with imperative /turtle/ style movement to build 
-- LocGraphics.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.LocTrace
  (

  -- * LocTrace monads
    LocTrace
  , LocTraceT

  , runLocTrace
  , evalLocTrace
  , execLocTrace

  , runLocTraceT
  , evalLocTraceT
  , execLocTraceT

  , liftLocTraceT

  -- * LocTrace classes
  , LocTraceM(..)
  , LocForkTraceM(..)

  -- * Derived operations
  , hmoveBy
  , vmoveBy

  )

  where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid

--
-- Note - there are no instances of DrawingCtxM for LocTrace or
-- LocTraceT. 
-- 
-- The type is not directly compatible as we are collecting 
-- LocGraphic in a triple, but also localize would change the 
-- interpretation of the vector cursor if the font-size changes.
--





-- | LocTrace is a writer state monad.
--
-- The writer accumulates a LocGraphic the state is a cumulative
-- displacement vector (called a cursor below).
--
newtype LocTrace u a = LocTrace { 
          getLocTrace :: Vec2 u -> (a, Vec2 u, LocGraphic u )}


type instance DUnit (LocTrace u a) = u


-- Do we need a transformer version 
-- | LocTrace is a writer state monad.
--
-- The writer accumulates a LocGraphic the state is a cumulative
-- displacement vector.
--
newtype LocTraceT u m a = LocTraceT { 
          getLocTraceT :: Vec2 u -> m (a, Vec2 u, LocGraphic u )}


type instance DUnit (LocTraceT u m a) = u


type instance MonUnit (LocTrace u a) = u
type instance MonUnit (LocTraceT u m a) = u


-- Functor

instance Functor (LocTrace u) where
  fmap f ma = LocTrace $ \v0 -> let (a,v1,o) = getLocTrace ma v0
                                in (f a, v1, o)

instance Monad m => Functor (LocTraceT u m) where
  fmap f ma = LocTraceT $ \v0 -> getLocTraceT ma v0 >>= \(a,v1,o) -> 
                                 return (f a, v1, o)

-- Applicative

instance Applicative (LocTrace u) where
  pure a    = LocTrace $ \v0 -> (a, v0, mempty)
  mf <*> ma = LocTrace $ \v0 -> 
                let (f,v1,o1) = getLocTrace mf v0
                    (a,v2,o2) = getLocTrace ma v1
                in (f a, v2, o1 `mappend` o2)




instance Monad m => Applicative (LocTraceT u m) where
  pure a    = LocTraceT $ \v0 -> return (a, v0, mempty)
  mf <*> ma = LocTraceT $ \v0 -> 
                getLocTraceT mf v0 >>= \(f,v1,o1) ->
                getLocTraceT ma v1 >>= \(a,v2,o2) ->
                return (f a, v2, o1 `mappend` o2)



-- Monad

instance Monad (LocTrace u) where
  return a  = LocTrace $ \v0 -> (a, v0, mempty)
  ma >>= k  = LocTrace $ \v0 -> 
                let (a,v1,o1) = getLocTrace ma v0
                    (b,v2,o2) = (getLocTrace . k) a v1
                in (b, v2, o1 `mappend` o2)

instance Monad m => Monad (LocTraceT u m) where
  return a  = LocTraceT $ \v0 -> return (a, v0, mempty)
  ma >>= k  = LocTraceT $ \v0 -> 
                getLocTraceT ma v0 >>= \(a,v1,o1) ->
                (getLocTraceT . k) a v1 >>= \(b,v2,o2) -> 
                return (b, v2, o1 `mappend` o2)



runLocTrace :: Num u => LocTrace u a -> (a, Vec2 u, LocGraphic u)
runLocTrace mf = getLocTrace mf (V2 0 0)


-- | Forget the generated LocImage, just return the /answer/.
--
evalLocTrace :: Num u => LocTrace u a -> a
evalLocTrace = post . runLocTrace
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated LocImage.
--
execLocTrace :: Num u => LocTrace u a -> LocGraphic u
execLocTrace = post . runLocTrace
  where
    post (_,_,o) = o



runLocTraceT :: (Monad m, Num u) 
             => LocTraceT u m a -> m (a, Vec2 u, LocGraphic u)
runLocTraceT mf = getLocTraceT mf (V2 0 0)


-- | Forget the generated LocImage, just return the /answer/.
--
evalLocTraceT :: (Monad m, Num u) => LocTraceT u m a -> m a
evalLocTraceT = liftM post . runLocTraceT
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated LocImage.
--
execLocTraceT :: (Monad m, Num u) => LocTraceT u m a -> m (LocGraphic u)
execLocTraceT = liftM post . runLocTraceT
  where
    post (_,_,o) = o



liftLocTraceT :: Monad m => m a -> LocTraceT u m a 
liftLocTraceT ma = LocTraceT $ \v0 -> 
                             ma >>= \a -> return (a,v0,mempty)




-- | 'insertl' analogue to Writer monad @tell@.
--
class Monad m => LocTraceM (m :: * -> *) where
  insertl   :: MonUnit (m ()) ~ u => LocGraphic u -> m ()
  insertl_  :: MonUnit (m ()) ~ u => LocImage u a -> m ()
  
  moveBy    :: MonUnit (m ()) ~ u => Vec2 u -> m ()
  location  :: MonUnit (m ()) ~ u => m (Vec2 u)


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
class LocTraceM m => LocForkTraceM (m :: * -> *) where
  reset     :: m ()

  -- Branch is like @local@ in the Reader monad.
  branch    :: m a -> m a





instance InterpretUnit u => LocTraceM (LocTrace u) where
  insertl gf  = LocTrace $ \v0 -> ((), v0, moveStart v0 gf)
  moveBy v    = LocTrace $ \v0 -> ((), v0 ^+^ v, mempty)
  location    = LocTrace $ \v0 -> (v0, v0, mempty)

instance InterpretUnit u => LocForkTraceM (LocTrace u) where
  reset       = LocTrace $ \_  -> ((), V2 0 0, mempty)
  branch ma   = LocTrace $ \v0 -> let (a,_,o) = getLocTrace ma v0 in (a,v0,o)
  

instance (Monad m, InterpretUnit u) => LocTraceM (LocTraceT u m) where
  insertl gf  = LocTraceT $ \v0 -> return ((), v0, moveStart v0 gf)
  moveBy v    = LocTraceT $ \v0 -> return ((), v0 ^+^ v, mempty)
  location    = LocTraceT $ \v0 -> return (v0, v0, mempty)

instance (LocTraceM m, InterpretUnit u) => LocForkTraceM (LocTraceT u m) where
  reset       = LocTraceT $ \_  -> return ((), V2 0 0, mempty)
  branch ma   = LocTraceT $ \v0 -> getLocTraceT ma v0 >>= \(a,_,o) -> 
                                   return (a,v0,o)


--------------------------------------------------------------------------------
-- Derived operations


-- | Move the /cursor/ horizontally.
--
hmoveBy :: (LocTraceM m, Num u, u ~ MonUnit (m ())) => u -> m ()
hmoveBy dx = moveBy (hvec dx)

-- | Move the /cursor/ vertically.
--
vmoveBy :: (LocTraceM m, Num u, u ~ MonUnit (m ())) => u -> m ()
vmoveBy dx = moveBy (vvec dx)
