{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.SnocDrawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- STrace plus DrawingCtx plus Turtle...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.SnocDrawing
  (
    SnocDrawing

  , runSnocDrawing
  , runSnocDrawingT
  , execSnocDrawing
  , execSnocDrawingT

  -- * Re-exports
  , module Wumpus.Basic.Monads.DrawingCtxClass
  , module Wumpus.Basic.Monads.TraceClass
  , module Wumpus.Basic.Monads.TurtleClass 

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.DrawingCtxMonad
import Wumpus.Basic.Monads.STraceMonad
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass
import Wumpus.Basic.Monads.TurtleMonad

import Wumpus.Core                      -- package: wumpus-core

import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative
import Control.Monad


newtype SnocDrawing u a = SnocDrawing { 
          getSnocDrawing  :: TurtleT           u
                           ( DrawingCtxT
                           ( STrace (Primitive u))) a }

newtype SnocDrawingT u m a = SnocDrawingT { 
          getSnocDrawingT :: TurtleT            u
                           ( DrawingCtxT
                           ( STraceT (Primitive u) m)) a }


-- Functor

instance Functor (SnocDrawing u) where
  fmap f = SnocDrawing . fmap f . getSnocDrawing

instance Monad m => Functor (SnocDrawingT u m) where
  fmap f = SnocDrawingT . fmap f . getSnocDrawingT


-- Applicative 
instance Applicative (SnocDrawing u) where
  pure a    = SnocDrawing $ pure a
  mf <*> ma = SnocDrawing $ getSnocDrawing mf <*> getSnocDrawing ma
                   


instance Monad m => Applicative (SnocDrawingT u m) where
  pure a    = SnocDrawingT $ pure a
  mf <*> ma = SnocDrawingT $ getSnocDrawingT mf <*> getSnocDrawingT ma


-- Monad 

instance Monad (SnocDrawing u) where
  return a = SnocDrawing $ return a
  m >>= k  = SnocDrawing $ getSnocDrawing m >>= (getSnocDrawing . k)


instance Monad m => Monad (SnocDrawingT u m) where
  return a = SnocDrawingT $ return a
  m >>= k  = SnocDrawingT $ getSnocDrawingT m >>= (getSnocDrawingT . k)


instance MonadT (SnocDrawingT u) where
  lift m = SnocDrawingT $ lift $ lift $ lift m


instance TurtleM (SnocDrawing u) u where
  getLoc   = SnocDrawing $ getLoc
  setLoc c = SnocDrawing $ setLoc c
  xStep    = SnocDrawing $ xStep
  yStep    = SnocDrawing $ yStep

instance Monad m => TurtleM (SnocDrawingT u m) u where
  getLoc   = SnocDrawingT $ getLoc
  setLoc c = SnocDrawingT $ setLoc c
  xStep    = SnocDrawingT $ xStep
  yStep    = SnocDrawingT $ yStep

instance DrawingCtxM (SnocDrawing u) where
  askDrawingCtx = SnocDrawing $ lift askDrawingCtx
  

instance Monad m => DrawingCtxM (SnocDrawingT u m) where
  askDrawingCtx = SnocDrawingT $ lift askDrawingCtx

instance TraceM (SnocDrawing u) (Primitive u) where
  trace  a = SnocDrawing $ lift $ lift $ trace a
  trace1 a = SnocDrawing $ lift $ lift $ trace1 a

instance Monad m => TraceM (SnocDrawingT u m) (Primitive u) where
  trace  a = SnocDrawingT $ lift $ lift $ trace a
  trace1 a = SnocDrawingT $ lift $ lift $ trace1 a

runSnocDrawing :: Num u 
               => TurtleConfig u 
               -> DrawingAttr 
               -> SnocDrawing u a 
               -> (a, Graphic u)
runSnocDrawing cfg attr mf = runSTrace 
                           ( runDrawingCtxT attr
                           ( evalTurtleT    cfg  $ getSnocDrawing mf ))


runSnocDrawingT :: (Monad m, Num u) 
                => TurtleConfig u 
                -> DrawingAttr 
                -> SnocDrawingT u m a 
                -> m (a, Graphic u)
runSnocDrawingT cfg attr mf = runSTraceT 
                            ( runDrawingCtxT attr
                            ( evalTurtleT    cfg  $ getSnocDrawingT mf ))

execSnocDrawing :: Num u 
                => TurtleConfig u 
                -> DrawingAttr 
                -> SnocDrawing u a 
                -> Graphic u
execSnocDrawing cfg attr mf = snd $ runSnocDrawing cfg attr mf


execSnocDrawingT :: (Monad m, Num u)
                 => TurtleConfig u 
                 -> DrawingAttr 
                 -> SnocDrawingT u m a 
                 -> m (Graphic u)
execSnocDrawingT cfg attr mf = liftM snd $ runSnocDrawingT cfg attr mf