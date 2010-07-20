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
    Projection
  , ScaleCtx(..)

  , CoordScale
  , CoordScaleT

  , CoordScaleM(..)
  
  , runCoordScale
  , runCoordScaleT

  ) where


import Wumpus.Core                      -- package: wumpus-core

import MonadLib

import Control.Applicative

type Projection ua u = ua -> u

data ScaleCtx ux uy u = ScaleCtx 
      { xproj   :: Projection ux u
      , yproj   :: Projection uy u
      }


newtype CoordScale  ux uy u   a = 
          CoordScale  { getCoordScale    :: ScaleCtx ux uy u -> a }
 
newtype CoordScaleT ux uy u m a = 
          CoordScaleT { getCoordScaleT   :: ScaleCtx ux uy u -> m a }




-- Functor

instance Functor (CoordScale ux uy u) where
  fmap f m = CoordScale $ \e -> let a = getCoordScale m e in f a


instance Monad m => Functor (CoordScaleT ux uy u m) where
  fmap f m = CoordScaleT $ \e -> getCoordScaleT m e >>= \a ->
                                 return (f a)


-- Applicative

instance Applicative (CoordScale ux uy u) where
  pure a    = CoordScale $ \_ -> a
  mf <*> ma = CoordScale $ \e -> let f = getCoordScale mf e 
                                     a = getCoordScale ma e
                                 in f a


instance Monad m => Applicative (CoordScaleT ux uy u m) where
  pure a    = CoordScaleT $ \_ -> return a
  mf <*> ma = CoordScaleT $ \e -> getCoordScaleT mf e >>= \f -> 
                                  getCoordScaleT ma e >>= \a ->
                                  return (f a)


-- Monad

instance Monad (CoordScale ux uy u) where
  return a = CoordScale $ \_ -> a
  m >>= k  = CoordScale $ \e -> let a = getCoordScale m e
                                in (getCoordScale . k) a e
     


instance Monad m => Monad (CoordScaleT ux uy u m) where
  return a = CoordScaleT $ \_ -> return a
  m >>= k  = CoordScaleT $ \e -> getCoordScaleT m e       >>= \a ->
                                 (getCoordScaleT . k) a e

--------------------------------------------------------------------------------


-- Note - this class is rather contrived so that it type checks
-- for the known instances - CoordScale and CoordScaleT - and 
-- maintains the relation between the unit parameters x y and u.
-- 

class CoordScaleM m ux uy u | m -> ux, m -> uy, m -> u where
  coordScale :: (ux,uy) -> m (Point2 u)
  xScale     :: ux -> m u
  yScale     :: uy -> m u


instance CoordScaleM (CoordScale ux uy u) ux uy u where       
  coordScale (x,y) = CoordScale $ \(ScaleCtx fx fy) -> P2 (fx x) (fy y)
  xScale x         = CoordScale $ \(ScaleCtx fx _ ) -> fx x
  yScale y         = CoordScale $ \(ScaleCtx _  fy) -> fy y




instance Monad m => CoordScaleM (CoordScaleT ux uy u m) ux uy u where
  coordScale (x,y) = CoordScaleT $ \(ScaleCtx fx fy) -> return $ P2 (fx x) (fy y)
  xScale x         = CoordScaleT $ \(ScaleCtx fx _ ) -> return $ fx x
  yScale y         = CoordScaleT $ \(ScaleCtx _  fy) -> return $ fy y



instance MonadT (CoordScaleT ux uy u) where
  lift m = CoordScaleT $ \_ -> m >>= \a -> return a



runCoordScale :: ScaleCtx ux uy u -> CoordScale ux uy u a -> a
runCoordScale ctx mf = getCoordScale mf ctx


runCoordScaleT :: Monad m => ScaleCtx ux uy u -> CoordScaleT ux uy u m a -> m a
runCoordScaleT ctx mf = getCoordScaleT mf ctx


