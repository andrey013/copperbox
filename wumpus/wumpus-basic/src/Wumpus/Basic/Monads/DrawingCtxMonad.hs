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

  -- * Re-exports from Wumpus.Basic.Graphic.DrawingAttr
    DrawingAttr(..)
  , standardAttr

  -- * DrawingCtx monads
  , DrawingCtx
  , DrawingCtxT     

  , DrawingCtxM(..)

  , runDrawingCtx
  , runDrawingCtxT

  , strokeAttr
  , fillAttr
  , textAttr
  , markHeight
  
  ) where

import Wumpus.Basic.Graphic.DrawingAttr ( DrawingAttr(..), standardAttr )
import qualified Wumpus.Basic.Graphic.DrawingAttr as DA

import Wumpus.Core                      -- package: wumpus-core

import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative
import Control.Monad


newtype DrawingCtx a = DrawingCtx  { getDrawingCtx  :: DA.DrawingAttr -> a } 

newtype DrawingCtxT m a = DrawingCtxT { getDrawingCtxT :: DA.DrawingAttr -> m a }


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


-- local to add? or new class...

class Monad m => DrawingCtxM m where
  askDrawingCtx :: m DrawingAttr


instance DrawingCtxM DrawingCtx where
  askDrawingCtx = DrawingCtx id
  

instance Monad m => DrawingCtxM (DrawingCtxT m) where
  askDrawingCtx = DrawingCtxT return


runDrawingCtx :: DrawingAttr -> DrawingCtx a -> a
runDrawingCtx cfg mf = getDrawingCtx mf cfg

runDrawingCtxT :: Monad m => DrawingAttr -> DrawingCtxT m a -> m a
runDrawingCtxT cfg mf = getDrawingCtxT mf cfg


strokeAttr  :: DrawingCtxM m => m (DRGB, StrokeAttr)
strokeAttr  = liftM DA.strokeAttr askDrawingCtx


fillAttr    :: DrawingCtxM m => m DRGB
fillAttr    = liftM DA.fillAttr askDrawingCtx

textAttr    :: DrawingCtxM m => m  (DRGB, FontAttr)
textAttr    = liftM DA.textAttr askDrawingCtx

markHeight  :: (Fractional u, DrawingCtxM m) => m u
markHeight  = liftM DA.markHeight askDrawingCtx

