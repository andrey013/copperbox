{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Score.Turtle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Turtle (or /Path-like/) score object.
--
-- Choice of @where-to-go-next@ is determined individually at each
-- event.
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Score.Turtle
  (

    renderTurtle
  , renderTurtleU
  , Turtle
  , TurtleT
  , DTurtle
  , DTurtleT

  , runTurtle
  , evalTurtle
  , execTurtle

  , runTurtleT
  , evalTurtleT
  , execTurtleT
  , liftTurtleT
  
  -- * Turtle classes
  , TurtleM(..)
  , LocForkTraceM(..)

  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Control.Monad
import Data.Monoid


renderTurtle :: InterpretUnit u 
             => Context ctx -> Turtle ctx u a -> Maybe RScore
renderTurtle ctx mf = 
    let PrimW ca _ = runEvent ctx $ applyLoc (execTurtle mf) 0
    in hprimToScoreMb $ singleH ca


renderTurtleU :: InterpretUnit u
              => Context ctx -> Turtle ctx u a -> RScore
renderTurtleU ctx mf = maybe fk id $ renderTurtle ctx mf
  where
    fk = error "renderTurtleU - empty score." 


newtype Turtle ctx u a = Turtle
          { getTurtle :: u -> (a, u, ULocEvent ctx u) }

newtype TurtleT ctx u m a = TurtleT 
          { getTurtleT :: u -> m (a, u, ULocEvent ctx u) }

type instance DUnit (Turtle ctx u a) = u
type instance UCtx  (Turtle ctx u)   = ctx

type instance DUnit (TurtleT ctx u m a) = u
type instance UCtx  (TurtleT ctx u m)   = ctx

type DTurtle ctx a    = Turtle ctx Double a
type DTurtleT ctx m a = TurtleT ctx Double m a



-- Functor 

instance Functor (Turtle ctx u) where
  fmap f ma = Turtle $ \s0 -> let (a,s1,w) = getTurtle ma s0 
                                in (f a, s1, w)


instance Monad m => Functor (TurtleT ctx u m) where
  fmap f ma = TurtleT $ \s0 -> getTurtleT ma s0 >>= \(a,s1,w) -> 
                                 return (f a, s1, w)


-- Applicative

instance Applicative (Turtle ctx u) where
  pure a    = Turtle $ \s0 -> (a, s0, mempty)
  mf <*> ma = Turtle $ \s0 -> 
                let (f,s1,w1) = getTurtle mf s0
                    (a,s2,w2) = getTurtle ma s1
                in (f a, s2, w1 `mappend` w2)


instance Monad m => Applicative (TurtleT ctx u m) where
  pure a    = TurtleT $ \s0 -> return (a, s0, mempty)
  mf <*> ma = TurtleT $ \s0 -> 
                getTurtleT mf s0 >>= \(f,s1,w1) ->
                getTurtleT ma s1 >>= \(a,s2,w2) ->
                return (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (Turtle ctx u) where
  return a  = Turtle $ \s0 -> (a, s0, mempty)
  ma >>= k  = Turtle $ \s0 -> 
                let (a,s1,w1) = getTurtle ma s0
                    (b,s2,w2) = (getTurtle . k) a s1
                in (b, s2, w1 `mappend` w2)



instance Monad m => Monad (TurtleT ctx u m) where
  return a  = TurtleT $ \s0 -> return (a, s0, mempty)
  ma >>= k  = TurtleT $ \s0 -> 
                getTurtleT ma s0      >>= \(a,s1,w1) ->
                (getTurtleT . k) a s1 >>= \(b,s2,w2) -> 
                return (b, s2, w1 `mappend` w2)



-- No instances for ContextM

-- Can (should?) Turtle support ContextM?
-- It would mean adding @\ctx ->@ to the definition.
--


-- Monoid

instance Monoid a => Monoid (Turtle ctx u a) where
  mempty           = Turtle $ \s0 -> (mempty, s0, mempty)
  ma `mappend` mb  = Turtle $ \s0 -> 
                       let (a,s1,w1) = getTurtle ma s0
                           (b,s2,w2) = getTurtle mb s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



instance (Monad m, Monoid a) => Monoid (TurtleT ctx u m a) where
  mempty           = TurtleT $ \s0 -> return (mempty, s0, mempty)
  ma `mappend` mb  = TurtleT $ \s0 -> 
                       getTurtleT ma s0 >>= \(a,s1,w1) ->
                       getTurtleT mb s1 >>= \(b,s2,w2) ->
                       return (a `mappend` b, s2, w1 `mappend` w2)


runTurtle :: Num u 
            => Turtle ctx u a -> (a, u, ULocEvent ctx u)
runTurtle mf = getTurtle mf 0

-- | Forget the generated LocEvent, just return the /answer/.
--
evalTurtle :: Num u => Turtle ctx u a -> a
evalTurtle = post . runTurtle
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated ULocEvent.
--
execTurtle :: Num u => Turtle ctx u a -> ULocEvent ctx u
execTurtle = post . runTurtle
  where
    post (_,_,o) = o




runTurtleT :: (Monad m, Num u) 
             => TurtleT ctx u m a -> m (a, u, ULocEvent ctx u)
runTurtleT mf = getTurtleT mf 0


-- | Forget the generated LocImage, just return the /answer/.
--
evalTurtleT :: (Monad m, Num u) => TurtleT ctx u m a -> m a
evalTurtleT = liftM post . runTurtleT
  where
    post (a,_,_) = a

-- | Forget the /answer/, just return the generated LocImage.
--
execTurtleT :: (Monad m, Num u) 
              => TurtleT ctx u m a -> m (ULocEvent ctx u)
execTurtleT = liftM post . runTurtleT
  where
    post (_,_,o) = o




liftTurtleT :: Monad m => m a -> TurtleT ctx u m a 
liftTurtleT ma = TurtleT $ \s0 -> 
                     ma >>= \a -> return (a,s0,mempty)


--------------------------------------------------------------------------------
-- TurtleM classes

-- | 'insertl' analogue to Writer monad @tell@.
--
class Monad m => TurtleM (m :: * -> *) where
  insertl   :: (u ~ DUnit (m ()), ctx ~ UCtx m) 
            => ULocEvent ctx u -> m ()
  insertl_  :: (u ~ DUnit (m ()), ctx ~ UCtx m) 
            => LocEvent ctx u a -> m ()
  
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
class TurtleM m => LocForkTraceM (m :: * -> *) where
  reset     :: m ()

  -- Branch is like @local@ in the Reader monad.
  branch    :: m a -> m a




-- TurtleM

instance InterpretUnit u => TurtleM (Turtle ctx u) where
  insertl gf  = Turtle $ \s0 -> ((), s0, moveStart s0 gf)
  moveBy v    = Turtle $ \s0 -> ((), s0 + v, mempty)
  location    = Turtle $ \s0 -> (s0, s0, mempty)


instance InterpretUnit u => LocForkTraceM (Turtle ctx u) where
  reset       = Turtle $ \_  -> ((), 0, mempty)
  branch ma   = Turtle $ \s0 -> 
                  let (a,_,o) = getTurtle ma s0 in (a,s0,o)



instance (Monad m, InterpretUnit u) => 
    TurtleM (TurtleT ctx u m) where
  insertl gf  = TurtleT $ \s0 -> 
                  return ((), s0, moveStart s0 gf)
  moveBy v    = TurtleT $ \s0 -> return ((), s0 + v, mempty)
  location    = TurtleT $ \s0 -> return (s0, s0, mempty)

instance (TurtleM m, InterpretUnit u) => 
    LocForkTraceM (TurtleT ctx u m) where
  reset       = TurtleT $ \_  -> return ((), 0, mempty)
  branch ma   = TurtleT $ \s0 -> getTurtleT ma s0 >>= \(a,_,o) -> 
                                   return (a,s0,o)

