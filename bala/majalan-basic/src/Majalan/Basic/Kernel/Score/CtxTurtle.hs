{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Score.CtxTurtle
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

module Majalan.Basic.Kernel.Score.CtxTurtle
  (
  
    Turtle
  , TurtleT
  , DTurtle
  , DTurtleT
  
  , runTurtle

  , liftTurtleT

  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Data.Monoid


-- Want to run a Turtle and get a ULocEvent 


newtype Turtle ctx u a = Turtle
          { getTurtle :: Context ctx -> OnsetDbl -> (a, OnsetDbl, CatPrim) }



newtype TurtleT ctx u m a = TurtleT 
          { getTurtleT :: Context ctx -> OnsetDbl -> m (a, OnsetDbl, CatPrim) }

type instance DUnit (Turtle ctx u a) = u
type instance UCtx  (Turtle ctx u)   = ctx

type instance DUnit (TurtleT ctx u m a) = u
type instance UCtx  (TurtleT ctx u m)   = ctx

type DTurtle ctx a    = Turtle ctx Double a
type DTurtleT ctx m a = TurtleT ctx Double m a



-- Functor 

instance Functor (Turtle ctx u) where
  fmap f ma = Turtle $ \r s -> let (a,s1,w) = getTurtle ma r s 
                               in (f a,s1,w)


instance Monad m => Functor (TurtleT ctx u m) where
  fmap f ma = TurtleT $ \r s -> getTurtleT ma r s >>= \(a,s1,w) -> 
                                 return (f a,s1,w)


-- Applicative

instance Applicative (Turtle ctx u) where
  pure a    = Turtle $ \_ s -> (a, s, mempty)
  mf <*> ma = Turtle $ \r s -> 
                let (f, s1, w1) = getTurtle mf r s
                    (a, s2, w2) = getTurtle ma r s1
                in (f a, s2, w1 `mappend` w2)


instance Monad m => Applicative (TurtleT ctx u m) where
  pure a    = TurtleT $ \_ s -> return (a, s, mempty)
  mf <*> ma = TurtleT $ \r s -> 
                getTurtleT mf r s  >>= \(f, s1, w1) ->
                getTurtleT ma r s1 >>= \(a, s2, w2) ->
                return (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (Turtle ctx u) where
  return a  = Turtle $ \_ s -> (a, s, mempty) 
  ma >>= k  = Turtle $ \r s -> 
                let (a, s1, w1) = getTurtle ma r s
                    (b, s2, w2) = (getTurtle . k) a r s1
                in (b, s2, w1 `mappend` w2)



instance Monad m => Monad (TurtleT ctx u m) where
  return a  = TurtleT $ \_ s -> return (a, s, mempty)
  ma >>= k  = TurtleT $ \r s -> 
                getTurtleT ma r s       >>= \(a, s1, w1) ->
                (getTurtleT . k) a r s1 >>= \(b, s2, w2) -> 
                return (b, s2, w1 `mappend` w2)


instance ContextM (Turtle ctx u) where
  askCtx          = Turtle $ \r s -> (r, s, mempty)
  asksCtx fn      = Turtle $ \r s -> (fn r, s, mempty)
  localize upd ma = Turtle $ \r s -> getTurtle ma (upd r) s


instance Monad m => ContextM (TurtleT ctx u m) where
  askCtx          = TurtleT $ \r s -> return (r, s, mempty)
  asksCtx fn      = TurtleT $ \r s -> return (fn r, s, mempty)
  localize upd ma = TurtleT $ \r s -> getTurtleT ma (upd r) s




-- Monoid

instance Monoid a => Monoid (Turtle ctx u a) where
  mempty           = Turtle $ \_ s -> (mempty, s, mempty)
  ma `mappend` mb  = Turtle $ \r s -> 
                       let (a,s1,w1) = getTurtle ma r s
                           (b,s2,w2) = getTurtle mb r s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



instance (Monad m, Monoid a) => Monoid (TurtleT ctx u m a) where
  mempty           = TurtleT $ \_ s -> return (mempty, s, mempty)
  ma `mappend` mb  = TurtleT $ \r s -> 
                       getTurtleT ma r s  >>= \(a,s1,w1) ->
                       getTurtleT mb r s1 >>= \(b,s2,w2) ->
                       return (a `mappend` b, s2, w1 `mappend` w2)



runTurtle :: InterpretUnit u => Turtle ctx u a -> LocEvent ctx u a
runTurtle mf = promoteLoc $ \ot -> 
                 askCtx  >>= \ctx ->
                 normalizeCtx ot >>= \dot -> 
                 let (a, _, ca) = getTurtle mf ctx dot 
                     evt        = primEvent ca
                 in replaceAns a $ evt

{-

-- Actually, I don\'t think there is a sensible run function for 
-- the transformer any more...


-- runTurtleT :: InterpretUnit u => TurtleT ctx u m a -> m (LocEvent ctx u a)
runTurtleT mf = promoteLoc $ \ot -> 
                 askCtx  >>= \ctx ->
                 normalizeCtx ot >>= \dot -> 
                 getTurtleT mf ctx dot >>= \(a,_, ca) ->
                 let evt        = primEvent ca
                 in replaceAns a $ evt
-}

{-


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

-}


liftTurtleT :: Monad m => m a -> TurtleT ctx u m a 
liftTurtleT ma = TurtleT $ \_ s -> ma >>= \a -> return (a,s,mempty)



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
  insertl gf  = Turtle $ \r s -> let sx = dinterp (ctx_tempo r) s
                                     PrimW ca _ = runEvent r (applyLoc gf sx)
                                 in ((), s, ca)

  moveBy v    = Turtle $ \r s -> let dv = normalize (ctx_tempo r) v
                                 in ((), s + dv, mempty)

  location    = Turtle $ \r s -> let sx = dinterp (ctx_tempo r) s
                                 in (sx, s, mempty)



instance InterpretUnit u => LocForkTraceM (Turtle ctx u) where
  reset       = Turtle $ \_ _ -> ((), 0, mempty)
  branch ma   = Turtle $ \r s -> 
                  let (a,_,o) = getTurtle ma r s in (a,s,o)


{-
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

-}