{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.ConsDrawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Trace plus DrawingCtx plus Turtle...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.ConsDrawing
  (
    ConsDrawing
  , ConsDrawingT

  , runConsDrawing
  , runConsDrawingT
  , execConsDrawing
  , execConsDrawingT

  -- * Re-exports
  , module Wumpus.Basic.Monads.Drawing
  , module Wumpus.Basic.Monads.DrawingCtxClass
  , module Wumpus.Basic.Monads.TraceClass
  , module Wumpus.Basic.Monads.TurtleClass 

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.DrawingCtxMonad
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TraceMonad
import Wumpus.Basic.Monads.TurtleClass
import Wumpus.Basic.Monads.TurtleMonad

import Wumpus.Core                      -- package: wumpus-core

import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative
import Control.Monad


newtype ConsDrawing u a = ConsDrawing { 
          getConsDrawing  :: TurtleT          u
                           ( DrawingCtxT
                           ( Trace (Primitive u))) a }

newtype ConsDrawingT u m a = ConsDrawingT { 
          getConsDrawingT :: TurtleT           u
                           ( DrawingCtxT
                           ( TraceT (Primitive u) m)) a }


-- Functor

instance Functor (ConsDrawing u) where
  fmap f = ConsDrawing . fmap f . getConsDrawing

instance Monad m => Functor (ConsDrawingT u m) where
  fmap f = ConsDrawingT . fmap f . getConsDrawingT


-- Applicative 
instance Applicative (ConsDrawing u) where
  pure a    = ConsDrawing $ pure a
  mf <*> ma = ConsDrawing $ getConsDrawing mf <*> getConsDrawing ma
                   


instance Monad m => Applicative (ConsDrawingT u m) where
  pure a    = ConsDrawingT $ pure a
  mf <*> ma = ConsDrawingT $ getConsDrawingT mf <*> getConsDrawingT ma


-- Monad 

instance Monad (ConsDrawing u) where
  return a = ConsDrawing $ return a
  m >>= k  = ConsDrawing $ getConsDrawing m >>= (getConsDrawing . k)


instance Monad m => Monad (ConsDrawingT u m) where
  return a = ConsDrawingT $ return a
  m >>= k  = ConsDrawingT $ getConsDrawingT m >>= (getConsDrawingT . k)


instance MonadT (ConsDrawingT u) where
  lift m = ConsDrawingT $ lift $ lift $ lift m


instance TurtleM (ConsDrawing u) where
  getLoc      = ConsDrawing $ getLoc
  setLoc c    = ConsDrawing $ setLoc c
  getOrigin   = ConsDrawing $ getOrigin
  setOrigin o = ConsDrawing $ setOrigin o

instance TurtleScaleM (ConsDrawing u) u where
  xStep    = ConsDrawing $ xStep
  yStep    = ConsDrawing $ yStep


instance Monad m => TurtleM (ConsDrawingT u m) where
  getLoc      = ConsDrawingT $ getLoc
  setLoc c    = ConsDrawingT $ setLoc c
  getOrigin   = ConsDrawingT $ getOrigin
  setOrigin o = ConsDrawingT $ setOrigin o


instance Monad m => TurtleScaleM (ConsDrawingT u m) u where
  xStep    = ConsDrawingT $ xStep
  yStep    = ConsDrawingT $ yStep


instance DrawingCtxM (ConsDrawing u) where
  askDrawingCtx   = ConsDrawing $ lift askDrawingCtx
  localCtx ctx ma = ConsDrawing $ localCtx ctx (getConsDrawing ma)
  

instance Monad m => DrawingCtxM (ConsDrawingT u m) where
  askDrawingCtx   = ConsDrawingT $ lift askDrawingCtx
  localCtx ctx ma = ConsDrawingT $ localCtx ctx (getConsDrawingT ma)

instance TraceM (ConsDrawing u) (Primitive u) where
  trace  a = ConsDrawing $ lift $ lift $ trace a
  trace1 a = ConsDrawing $ lift $ lift $ trace1 a

instance Monad m => TraceM (ConsDrawingT u m) (Primitive u) where
  trace  a = ConsDrawingT $ lift $ lift $ trace a
  trace1 a = ConsDrawingT $ lift $ lift $ trace1 a

runConsDrawing :: Num u 
               => TurtleConfig u 
               -> (Int,Int)
               -> DrawingAttr 
               -> ConsDrawing u a 
               -> (a, Graphic u)
runConsDrawing cfg ogin attr mf = runTrace 
                                ( runDrawingCtxT attr
                                ( runTurtleT cfg ogin $ getConsDrawing mf ))


runConsDrawingT :: (Monad m, Num u) 
                => TurtleConfig u 
                -> (Int,Int)
                -> DrawingAttr 
                -> ConsDrawingT u m a 
                -> m (a, Graphic u)
runConsDrawingT cfg ogin attr mf = runTraceT 
                                 ( runDrawingCtxT attr
                                 ( runTurtleT cfg ogin $ getConsDrawingT mf ))

execConsDrawing :: Num u 
                => TurtleConfig u
                -> (Int,Int) 
                -> DrawingAttr 
                -> ConsDrawing u a 
                -> Graphic u
execConsDrawing cfg ogin attr mf = snd $ runConsDrawing cfg ogin attr mf


execConsDrawingT :: (Monad m, Num u)
                 => TurtleConfig u 
                 -> (Int,Int)
                 -> DrawingAttr 
                 -> ConsDrawingT u m a 
                 -> m (Graphic u)
execConsDrawingT cfg ogin attr mf = liftM snd $ runConsDrawingT cfg ogin attr mf