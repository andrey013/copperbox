{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.DrawingMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Drawing with trace and drawing context (i.e. reader monad
-- of attributes - fill_colour etc.).
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.DrawingMonad
  (
    
    Drawing
  , DrawingT
  , runDrawing
  , execDrawing
  , runDrawingT
  , execDrawingT

  , module Wumpus.Basic.Monads.DrawingCtxClass
  , module Wumpus.Basic.Monads.TraceClass

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass


-- import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad



newtype Drawing  u a   = Drawing { 
          getDrawing :: DrawingAttr -> (a, Graphic u) }

newtype DrawingT u m a = DrawingT { 
          getDrawingT :: DrawingAttr -> m (a, Graphic u) }

-- Functor

instance Functor (Drawing u) where
  fmap f ma = Drawing $ \attr -> 
                let (a,w) = getDrawing ma attr in (f a,w)


instance Monad m => Functor (DrawingT u m) where
  fmap f ma = DrawingT $ \attr -> 
                getDrawingT ma attr >>= \(a,w) -> return (f a,w)

-- Applicative

instance Applicative (Drawing u) where
  pure a    = Drawing $ \_    -> (a, emptyG)
  mf <*> ma = Drawing $ \attr -> let (f,w1) = getDrawing mf attr 
                                     (a,w2) = getDrawing ma attr
                                 in (f a, w2 . w1)

instance Monad m => Applicative (DrawingT u m) where
  pure a    = DrawingT $ \_    -> return (a, emptyG)
  mf <*> ma = DrawingT $ \attr -> getDrawingT mf attr >>= \(f,w1) ->
                                  getDrawingT ma attr >>= \(a,w2) ->
                                  return (f a, w2 . w1)

-- Monad

instance Monad (Drawing u) where
  return a  = Drawing $ \_    -> (a, emptyG)
  ma >>= k  = Drawing $ \attr -> let (a,w1) = getDrawing ma attr 
                                     (b,w2) = (getDrawing . k) a attr
                                 in (b, w2 . w1)



instance Monad m => Monad (DrawingT u m) where
  return a  = DrawingT $ \_    -> return (a, emptyG)
  ma >>= k  = DrawingT $ \attr -> getDrawingT ma attr      >>= \(a,w1) ->
                                  (getDrawingT . k) a attr >>= \(b,w2) -> 
                                  return (b, w2 . w1)

-- TraceM 

instance TraceM (Drawing u) u where
  trace a = Drawing $ \_ -> ((),a)

instance Monad m => TraceM (DrawingT u m) u where
  trace a = DrawingT $ \_ -> return ((),a)


-- DrawingCtxM

instance DrawingCtxM (Drawing u) where
  askDrawingCtx   = Drawing $ \attr -> (attr,emptyG)
  localCtx ctx ma = Drawing $ \_    -> getDrawing ma ctx



instance Monad m => DrawingCtxM (DrawingT u m) where
  askDrawingCtx   = DrawingT $ \attr -> return (attr,emptyG)
  localCtx ctx ma = DrawingT $ \_    -> getDrawingT ma ctx





runDrawing :: DrawingAttr -> Drawing u a -> (a, Graphic u) 
runDrawing attr ma = getDrawing ma attr

execDrawing :: DrawingAttr -> Drawing u a -> Graphic u
execDrawing attr ma = snd $ runDrawing attr ma

runDrawingT :: Monad m => DrawingAttr -> DrawingT u m a -> m (a, Graphic u) 
runDrawingT attr ma = getDrawingT ma attr

execDrawingT :: Monad m => DrawingAttr -> DrawingT u m a -> m (Graphic u)
execDrawingT attr ma = liftM snd $ runDrawingT attr ma
