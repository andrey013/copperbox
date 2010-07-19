{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.ScaleMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Coordinate scaling (evironment) monad ...
--
--------------------------------------------------------------------------------

module Wumpus.PSC.ScaleMonad
  ( 
    ScaleCtx(..)

  , CoordScale
  , CoordScaleT

  , CoordScaleM(..)
  
  , runCoordScale
  , runCoordScaleT

  ) where


import Wumpus.Core                      -- package: wumpus-core

import MonadLib

import Control.Applicative

type Projection a u = a -> u

data ScaleCtx x y u = ScaleCtx 
      { xproj   :: Projection x u
      , yproj   :: Projection y u
      }


newtype CoordScale   x y u    a = CoordScale  { 
                                   getCoordScale    :: ScaleCtx x y u -> a }
 
newtype CoordScaleT  x y u m  a = CoordScaleT { 
                                   getCoordScaleT   :: ScaleCtx x y u -> m a }




-- Functor

instance Functor (CoordScale x y u) where
  fmap f m = CoordScale $ \e -> let a = getCoordScale m e in f a


instance Monad m => Functor (CoordScaleT x y u m) where
  fmap f m = CoordScaleT $ \e -> getCoordScaleT m e >>= \a ->
                                 return (f a)


-- Applicative

instance Applicative (CoordScale x y u) where
  pure a    = CoordScale $ \_ -> a
  mf <*> ma = CoordScale $ \e -> let f = getCoordScale mf e 
                                     a = getCoordScale ma e
                                 in f a


instance Monad m => Applicative (CoordScaleT x y u m) where
  pure a    = CoordScaleT $ \_ -> return a
  mf <*> ma = CoordScaleT $ \e -> getCoordScaleT mf e >>= \f -> 
                                  getCoordScaleT ma e >>= \a ->
                                  return (f a)


-- Monad

instance Monad (CoordScale x y u) where
  return a = CoordScale $ \_ -> a
  m >>= k  = CoordScale $ \e -> let a = getCoordScale m e
                                in (getCoordScale . k) a e
     


instance Monad m => Monad (CoordScaleT x y u m) where
  return a = CoordScaleT $ \_ -> return a
  m >>= k  = CoordScaleT $ \e -> getCoordScaleT m e       >>= \a ->
                                 (getCoordScaleT . k) a e

--------------------------------------------------------------------------------


-- Note - this class is rather contrived so that it type checks
-- for the known instances - CoordScale and CoordScaleT - and 
-- maintains the relation between the unit parameters x y and u.
-- 

class CoordScaleM m x y u | m -> x, m -> y, m -> u where
  coordScale :: (x,y) -> m (Point2 u)



instance CoordScaleM (CoordScale x y u) x y u where       
  coordScale (x,y) = CoordScale $ \(ScaleCtx fx fy) -> P2 (fx x) (fy y)



instance Monad m => CoordScaleM (CoordScaleT x y u m) x y u where
  coordScale (x,y) = CoordScaleT $ \(ScaleCtx fx fy) -> return $ P2 (fx x) (fy y)


instance MonadT (CoordScaleT x y u) where
  lift m = CoordScaleT $ \_ -> m >>= \a -> return a



runCoordScale :: ScaleCtx x y u -> CoordScale x y u a -> a
runCoordScale ctx mf = getCoordScale mf ctx


runCoordScaleT :: Monad m => ScaleCtx x y u -> CoordScaleT x y u m a -> m a
runCoordScaleT ctx mf = getCoordScaleT mf ctx
