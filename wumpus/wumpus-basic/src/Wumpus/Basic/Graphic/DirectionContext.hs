{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.DirectionContext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Reader monad over (angular) direction.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.DirectionContext
  (

    Direction
  , runDirection
  , DirectionT
  , runDirectionT

  , displacePerp
  , displacePara

  ) where

import Wumpus.Basic.Graphic.Base

import Wumpus.Core				-- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative



--
newtype Direction a = Direction { getDirection :: Radian -> a }



instance Functor Direction where
  fmap f ma = Direction $ \r -> let a = getDirection ma r in f a

instance Applicative Direction where
  pure a    = Direction $ \_ -> a
  mf <*> ma = Direction $ \r -> let f = getDirection mf r
                                    a = getDirection ma r
             			in (f a)

instance Monad Direction where
  return a = Direction $ \_ -> a
  m >>= k  = Direction $ \r -> let a = getDirection m r
    	     	               in (getDirection . k) a r



runDirection :: Radian -> Direction a -> a
runDirection theta sf = (getDirection sf) theta


instance DirectionM Direction where
  localTheta theta ma = Direction $ \_ -> getDirection ma theta
  asksTheta fn        = Direction $ \r -> fn r
  parallel d          = Direction $ \r -> avec (circularModulo r) d
  perpendicular d     = Direction $ \r -> 
                          avec (circularModulo $ (0.5*pi) + r) d  


--------------------------------------------------------------------------------
-- Transformer

newtype DirectionT m a = DirectionT { getDirectionT :: Radian -> m a }

type instance MonUnit (DirectionT m) = MonUnit m

instance Monad m => Functor (DirectionT m) where
  fmap f ma = DirectionT $ \r -> getDirectionT ma r >>= \a -> return (f a)

instance Monad m => Applicative (DirectionT m) where
  pure a    = DirectionT $ \_ -> return a
  mf <*> ma = DirectionT $ \r -> getDirectionT mf r >>= \f -> 
                                 getDirectionT ma r >>= \a ->
             			 return (f a)

instance Monad m => Monad (DirectionT m) where
  return a = DirectionT $ \_ -> return a
  m >>= k  = DirectionT $ \r -> getDirectionT m r >>= \a -> 
    	     	      	     	(getDirectionT . k) a r



instance Monad m => DirectionM (DirectionT m) where
  localTheta theta ma = DirectionT $ \_ -> getDirectionT ma theta
  asksTheta fn        = DirectionT $ \r -> return (fn r)
  parallel d          = DirectionT $ \r -> return (avec (circularModulo r) d)
  perpendicular d     = DirectionT $ \r -> 
                          return (avec (circularModulo $ (0.5*pi) + r) d)


-- Cross instances - needed to run SalingT /locally/ in Drawing.

instance DrawingCtxM m => DrawingCtxM (DirectionT m) where
  askDC           = DirectionT $ \_ -> askDC >>= \dctx -> return dctx
  localize ctx mf = DirectionT $ \r -> localize ctx (getDirectionT mf r)


instance (Monad m, TraceM m) => TraceM (DirectionT m) where
  trace a  = DirectionT $ \_ -> trace a 





runDirectionT :: Radian -> DirectionT m a -> m a
runDirectionT theta sf = (getDirectionT sf) theta


--------------------------------------------------------------------------------

displacePerp :: (DirectionM m, Floating u) => u -> Point2 u -> m (Point2 u)
displacePerp u pt = perpendicular u >>= \v -> return (pt .+^ v)


displacePara :: (DirectionM m, Floating u) => u -> Point2 u -> m (Point2 u)
displacePara u pt = parallel u >>= \v -> return (pt .+^ v)