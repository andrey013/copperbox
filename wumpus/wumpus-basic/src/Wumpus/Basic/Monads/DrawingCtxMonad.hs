{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.DrawingCtxMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Reader (enviroment) monad for common drawing attributes.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.DrawingCtxMonad
  (


  -- * DrawingCtx monads
    DrawingCtx
  , DrawingCtxT     

  , runDrawingCtx
  , runDrawingCtxT

  
  ) where

import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass

import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative
import Control.Monad


newtype DrawingCtx a = DrawingCtx  { getDrawingCtx  :: DrawingAttr -> a } 

newtype DrawingCtxT m a = DrawingCtxT { getDrawingCtxT :: DrawingAttr -> m a }


-- Functor

instance Functor DrawingCtx where
  fmap f m = DrawingCtx $ \r -> let a = getDrawingCtx m r in f a

instance Monad m => Functor (DrawingCtxT m) where
  fmap f m = DrawingCtxT $ \r -> getDrawingCtxT m r >>= \a ->
                                 return (f a)

-- Applicative 
instance Applicative DrawingCtx where
  pure a    = DrawingCtx $ \_ -> a
  mf <*> ma = DrawingCtx $ \r -> let f = getDrawingCtx mf r
                                     a = getDrawingCtx ma r
                                 in f a 


instance Monad m => Applicative (DrawingCtxT m) where
  pure a    = DrawingCtxT $ \_ -> return a
  mf <*> ma = DrawingCtxT $ \r -> getDrawingCtxT mf r  >>= \f ->
                                  getDrawingCtxT ma r  >>= \a ->
                                  return (f a) 


-- Monad 

instance Monad DrawingCtx where
  return a = DrawingCtx $ \_ -> a
  m >>= k  = DrawingCtx $ \r -> let a = getDrawingCtx m r
                                in (getDrawingCtx . k) a r

instance Monad m => Monad (DrawingCtxT m) where
  return a = DrawingCtxT $ \_ -> return a
  m >>= k  = DrawingCtxT $ \r -> getDrawingCtxT m r       >>= \a  ->
                                 (getDrawingCtxT . k) a r 


instance MonadT DrawingCtxT where
  lift m = DrawingCtxT $ \_ -> m >>= \a -> return a



instance DrawingCtxM DrawingCtx where
  askDrawingCtx    = DrawingCtx id
  localCtx ctx ma  = DrawingCtx $ \_ -> getDrawingCtx ma ctx  

instance Monad m => DrawingCtxM (DrawingCtxT m) where
  askDrawingCtx    = DrawingCtxT return
  localCtx ctx ma  = DrawingCtxT $ \_ -> getDrawingCtxT ma ctx  


runDrawingCtx :: DrawingAttr -> DrawingCtx a -> a
runDrawingCtx cfg mf = getDrawingCtx mf cfg

runDrawingCtxT :: Monad m => DrawingAttr -> DrawingCtxT m a -> m a
runDrawingCtxT cfg mf = getDrawingCtxT mf cfg

--------------------------------------------------------------------------------
--- Cross instances

instance (Monad m, TraceM m i) => TraceM (DrawingCtxT m) i where
  trace a  = DrawingCtxT $ \_ -> trace a
  trace1 a = DrawingCtxT $ \_ -> trace1 a

instance TurtleM m => TurtleM (DrawingCtxT m) where
  getLoc          = DrawingCtxT $ \_ -> getLoc
  setLoc c        = DrawingCtxT $ \_ -> setLoc c
  getOrigin       = DrawingCtxT $ \_ -> getOrigin
  setOrigin o     = DrawingCtxT $ \_ -> setOrigin o

instance TurtleScaleM m u => TurtleScaleM (DrawingCtxT m) u where
  xStep           = DrawingCtxT $ \_ -> xStep
  yStep           = DrawingCtxT $ \_ -> yStep
