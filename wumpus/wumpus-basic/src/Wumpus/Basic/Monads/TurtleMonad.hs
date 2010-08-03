{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.TurtleMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Turtle monad and monad transformer.
--
-- The Turtle monad embodies the LOGO style of imperative 
-- drawing - sending commands to update the a cursor.
--
-- While Wumpus generally aims for a more compositional,
-- \"coordinate-free\" style of drawing, some types of 
-- diagram are more easily expressed in the LOGO style.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.TurtleMonad
  (


    Turtle
  , TurtleT

  , runTurtle
  , runTurtleT

  , evalTurtle
  , evalTurtleT 

 
  ) where

import Wumpus.Basic.Monads.TurtleClass

import Wumpus.Core                      -- package: wumpus-core

import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative
import Control.Monad


-- Turtle is a Reader / State monad
-- 
-- The env is the horizontal and vertical move distances.
-- 
-- The state is the current point.
--

newtype Turtle u a = Turtle  { 
          getTurtle  :: TurtleConfig u -> Point2 u -> (a, Point2 u) } 

newtype TurtleT u m a = TurtleT { 
          getTurtleT :: TurtleConfig u -> Point2 u -> m (a, Point2 u) }


-- Functor

instance Functor (Turtle u) where
  fmap f m = Turtle $ \r s -> let (a,s') = getTurtle m r s in (f a, s')

instance Monad m => Functor (TurtleT u m) where
  fmap f m = TurtleT $ \r s -> getTurtleT m r s >>= \(a,s') ->
                               return (f a, s')

-- Applicative 
instance Applicative (Turtle u) where
  pure a    = Turtle $ \_ s -> (a,s)
  mf <*> ma = Turtle $ \r s -> let (f,s')  = getTurtle mf r s 
                                   (a,s'') = getTurtle ma r s'
                                in (f a,s'') 


instance Monad m => Applicative (TurtleT u m) where
  pure a    = TurtleT $ \_ s -> return (a,s)
  mf <*> ma = TurtleT $ \r s -> getTurtleT mf r s  >>= \(f,s')  ->
                                getTurtleT ma r s' >>= \(a,s'') ->
                                return (f a,s'') 


-- Monad 

instance Monad (Turtle u) where
  return a = Turtle $ \_ s -> (a,s)
  m >>= k  = Turtle $ \r s -> let (a,s') = getTurtle m r s
                              in (getTurtle . k) a r s'

instance Monad m => Monad (TurtleT u m) where
  return a = TurtleT $ \_ s -> return (a,s)
  m >>= k  = TurtleT $ \r s -> getTurtleT m r s        >>= \(a,s')  ->
                               (getTurtleT . k) a r s' >>= \(b,s'') ->
                               return (b,s'')

instance MonadT (TurtleT u) where
  lift m = TurtleT $ \_ s -> m >>= \a -> return (a,s)



instance TurtleM (Turtle u) u where
  getLoc   = Turtle $ \_ s -> (s,s)
  setLoc c = Turtle $ \_ _ -> ((),c)
  xStep    = Turtle $ \r s -> (xstep r,s)
  yStep    = Turtle $ \r s -> (ystep r,s)


instance Monad m => TurtleM (TurtleT u m) u where
  getLoc   = TurtleT $ \_ s -> return (s,s)
  setLoc c = TurtleT $ \_ _ -> return ((),c)
  xStep    = TurtleT $ \r s -> return (xstep r,s)
  yStep    = TurtleT $ \r s -> return (ystep r,s)


runTurtle :: Num u => TurtleConfig u -> Turtle u a -> (a, Point2 u)
runTurtle cfg mf = getTurtle mf cfg zeroPt 
 
runTurtleT :: (Monad m, Num u) 
           => TurtleConfig u -> TurtleT u m a -> m (a, Point2 u)
runTurtleT cfg mf = getTurtleT mf cfg zeroPt

evalTurtle :: Num u => TurtleConfig u -> Turtle u a -> a
evalTurtle cfg mf = fst $ runTurtle cfg mf 

evalTurtleT :: (Monad m, Num u) 
            => TurtleConfig u -> TurtleT u m a -> m a
evalTurtleT cfg mf = liftM fst $ runTurtleT cfg mf


