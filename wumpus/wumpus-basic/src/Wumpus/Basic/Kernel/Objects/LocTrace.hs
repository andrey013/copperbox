{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocTrace
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Imperative /turtle/ style drawing to build LocGraphics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocTrace
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

   -- * LocTrace class
   , LocTraceM(..)

   -- * Derived operations
   , hmove
   , vmove

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid

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

instance Functor (LocTrace u) where
  fmap f ma = LocTrace $ \v0 -> let (a,v1,o) = getLocTrace ma v0
                                in (f a, v1, o)

instance Applicative (LocTrace u) where
  pure a    = LocTrace $ \v0  -> (a, v0, mempty)
  mf <*> ma = LocTrace $ \v0 -> 
                let (f,v1,o1) = getLocTrace mf v0
                    (a,v2,o2) = getLocTrace ma v1
                in (f a, v2, o1 `mappend` o2)

instance Monad (LocTrace u) where
  return a  = LocTrace $ \v0  -> (a, v0, mempty)
  ma >>= k  = LocTrace $ \v0 -> 
                let (a,v1,o1) = getLocTrace ma v0
                    (b,v2,o2) = (getLocTrace . k) a v1
                in (b, v2, o1 `mappend` o2)


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



-- | 'write' analogue to Writer monad @tell@.
--
class LocTraceM (m :: * -> *) where
  write     :: MonUnit (m ()) ~ u => LocGraphic u -> m ()
  write_    :: MonUnit (m ()) ~ u => LocImage u a -> m ()
  
  move      :: MonUnit (m ()) ~ u => Vec2 u -> m ()
  location  :: MonUnit (m ()) ~ u => m (Vec2 u)

  reset     :: m ()
  
  -- Branch is like @local@ in the Reader monad.
  branch    :: m a -> m a


  write_ = write . locGraphic_ 


instance Num u => LocTraceM (LocTrace u) where
  write gf  = LocTrace $ \v0 -> ((), v0, moveStart (displaceVec v0) gf)
  move  v   = LocTrace $ \v0 -> ((), v0 ^+^ v, mempty)
  location  = LocTrace $ \v0 -> (v0, v0, mempty)
  reset     = LocTrace $ \_  -> ((), V2 0 0, mempty)
  branch ma = LocTrace $ \v0 -> let (a,_,o) = getLocTrace ma v0 in (a,v0,o)
  

instance (Monad m, Num u) => LocTraceM (LocTraceT u m) where
  write gf  = LocTraceT $ \v0 -> return ((), v0, moveStart (displaceVec v0) gf)
  move  v   = LocTraceT $ \v0 -> return ((), v0 ^+^ v, mempty)
  location  = LocTraceT $ \v0 -> return (v0, v0, mempty)
  reset     = LocTraceT $ \_  -> return ((), V2 0 0, mempty)
  branch ma = LocTraceT $ \v0 -> getLocTraceT ma v0 >>= \(a,_,o) -> 
                                 return (a,v0,o)


--------------------------------------------------------------------------------
-- Derived operations


-- | Move the /cursor/ horizontally.
--
hmove :: (LocTraceM m, Num u, u ~ MonUnit (m ())) => u -> m ()
hmove dx = move (hvec dx)

-- | Move the /cursor/ vertically.
--
vmove :: (LocTraceM m, Num u, u ~ MonUnit (m ())) => u -> m ()
vmove dx = move (vvec dx)
