{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.TraceGraphic
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Build multi-part Graphics with an accumulator (i.e. a Writer 
-- monad).
--
-- Note - the run functions for the transformer and the plain
-- monad are quite different. This mandated by the need to  
-- single-thread the DrawingContext through the transformer.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.TraceGraphic
  (
    TraceGraphic
  , TraceGraphicT

  , TraceGraphicM(..)

  , runTraceGraphic
  , runTraceGraphicT

  , liftTraceGraphicT

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image

import Control.Applicative
import Data.Monoid



newtype TraceGraphic u a = TraceGraphic { 
    getTraceGraphic :: DrawingContext -> (a, CatPrim) }


newtype TraceGraphicT u m a = TraceGraphicT { 
    getTraceGraphicT :: DrawingContext -> m (a, CatPrim) }


type instance MonUnit (TraceGraphic u a)    = u
type instance MonUnit (TraceGraphicT u m a) = u


-- Functor

instance Functor (TraceGraphic u) where
  fmap f ma = TraceGraphic $ \ctx -> let (a,w1) = getTraceGraphic ma ctx
                                     in (f a,w1)


instance Monad m => Functor (TraceGraphicT u m) where
  fmap f ma = TraceGraphicT $ \ctx -> getTraceGraphicT ma ctx >>= \(a,w1) -> 
                                      return (f a,w1)


-- Applicative

instance Applicative (TraceGraphic u) where
  pure a    = TraceGraphic $ \_   -> (a, mempty)
  mf <*> ma = TraceGraphic $ \ctx -> 
                let (f,w1) = getTraceGraphic mf ctx
                    (a,w2) = getTraceGraphic ma ctx
                in (f a,w1 `mappend` w2)


instance Monad m => Applicative (TraceGraphicT u m) where
  pure a    = TraceGraphicT $ \_   -> return (a, mempty)
  mf <*> ma = TraceGraphicT $ \ctx -> 
                getTraceGraphicT mf ctx >>= \(f,w1) ->
                getTraceGraphicT ma ctx >>= \(a,w2) -> 
                return (f a,w1 `mappend` w2)



-- Monad

instance Monad (TraceGraphic u) where
  return a  = TraceGraphic $ \_   -> (a, mempty)
  ma >>= k  = TraceGraphic $ \ctx -> 
                let (a,w1) = getTraceGraphic ma ctx
                    (b,w2) = (getTraceGraphic . k) a ctx
                in (b, w1 `mappend` w2)
               

instance Monad m => Monad (TraceGraphicT u m) where
  return a  = TraceGraphicT $ \_   -> return (a, mempty)
  ma >>= k  = TraceGraphicT $ \ctx -> 
                getTraceGraphicT ma ctx      >>= \(a,w1) -> 
                (getTraceGraphicT . k) a ctx >>= \(b,w2) -> 
                return (b, w1 `mappend` w2)
               


-- DrawingCtxM

instance DrawingCtxM (TraceGraphic u) where
  askDC           = TraceGraphic $ \ctx -> (ctx, mempty)
  asksDC f        = TraceGraphic $ \ctx -> (f ctx, mempty)
  localize upd ma = TraceGraphic $ \ctx -> getTraceGraphic ma (upd ctx)



instance Monad m => DrawingCtxM (TraceGraphicT u m) where
  askDC           = TraceGraphicT $ \ctx -> return (ctx, mempty)
  asksDC f        = TraceGraphicT $ \ctx -> return (f ctx, mempty)
  localize upd ma = TraceGraphicT $ \ctx -> getTraceGraphicT ma (upd ctx)



-- TraceGraphicM

class Monad m => TraceGraphicM m where
  tellImage   :: MonUnit (m ()) ~ u => Image u a -> m a
  tellImage_  :: MonUnit (m ()) ~ u => Image u a -> m ()

  tellImage_ ma = tellImage ma >> return ()


instance TraceGraphicM (TraceGraphic u) where
  tellImage  img = TraceGraphic $ \ctx -> 
                     let (Ans o a) = runCF ctx img in (a,o)

  tellImage_ img = TraceGraphic $ \ctx -> 
                     let (Ans o _) = runCF ctx img in ((),o)


instance Monad m => TraceGraphicM (TraceGraphicT u m) where
  tellImage  img = TraceGraphicT $ \ctx ->  
                     let (Ans o a) = runCF ctx img in return (a,o) 

  tellImage_ img = TraceGraphicT $ \ctx -> 
                     let (Ans o _) = runCF ctx img in return ((),o)


runTraceGraphic :: TraceGraphic u a -> Image u a
runTraceGraphic mf = askDC >>= \ctx -> let (a,o) = getTraceGraphic mf ctx
                                       in return (Ans o a)


-- | Note - this needs DrawingContext as an explicit parameter,
-- and hence it returns a pair of @(a, HPrim u)@ rather than an
-- Image.
--
-- It is expected this will be wrapped in to form a specific
-- TraceDrawing /draw/ function for the amalgamated monad.
-- 
runTraceGraphicT :: Monad m 
                 => DrawingContext -> TraceGraphicT u m a -> m (a, HPrim u)
runTraceGraphicT ctx mf = 
    getTraceGraphicT mf ctx >>= \(a,o) -> return (a, singleH o)


liftTraceGraphicT :: Monad m => m a -> TraceGraphicT u m a 
liftTraceGraphicT ma = TraceGraphicT $ \_ -> ma >>= \a -> return (a,mempty)

