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

 
  ) where

import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TurtleClass


import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative
import Control.Monad


-- Turtle is a Reader / State monad
-- 
-- The env is the horizontal and vertical move distances.
-- 
-- The state is the current coordinate and the origin.
--

data TurtleState = TurtleState 
      { _turtle_origin   :: (Int,Int)
      , _current_coord   :: (Int,Int)
      }

newtype Turtle u a = Turtle  { 
          getTurtle  :: TurtleConfig u -> TurtleState -> (a, TurtleState) } 

newtype TurtleT u m a = TurtleT { 
          getTurtleT :: TurtleConfig u -> TurtleState -> m (a, TurtleState) }


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



instance TurtleM (Turtle u) where
  getLoc      = Turtle $ \_ s@(TurtleState _ c) -> (c,s)
  setLoc c    = Turtle $ \_   (TurtleState o _) -> ((),TurtleState o c)
  getOrigin   = Turtle $ \_ s@(TurtleState o _) -> (o,s)
  setOrigin o = Turtle $ \_   (TurtleState _ c) -> ((),TurtleState o c)

instance TurtleScaleM (Turtle u) u where
  xStep    = Turtle $ \r s -> (xstep r,s)
  yStep    = Turtle $ \r s -> (ystep r,s)


instance Monad m => TurtleM (TurtleT u m) where
  getLoc      = TurtleT $ \_ s@(TurtleState _ c) -> return (c,s)
  setLoc c    = TurtleT $ \_   (TurtleState o _) -> return ((),TurtleState o c)
  getOrigin   = TurtleT $ \_ s@(TurtleState o _) -> return (o,s)
  setOrigin o = TurtleT $ \_   (TurtleState _ c) -> return ((),TurtleState o c)

instance Monad m => TurtleScaleM (TurtleT u m) u where
  xStep    = TurtleT $ \r s -> return (xstep r,s)
  yStep    = TurtleT $ \r s -> return (ystep r,s)


-- Run functions discard the state...

runTurtle :: Num u => TurtleConfig u -> (Int,Int) -> Turtle u a -> a
runTurtle cfg ogin mf = fst $ getTurtle mf cfg (TurtleState ogin ogin)
 
runTurtleT :: (Monad m, Num u) 
           => TurtleConfig u -> (Int,Int) -> TurtleT u m a -> m a
runTurtleT cfg ogin mf = liftM fst $ getTurtleT mf cfg (TurtleState ogin ogin)



----------------------------------------------------------------------------------

instance DrawingCtxM m => DrawingCtxM (TurtleT u m) where
  askDrawingCtx   = TurtleT $ \_ s -> askDrawingCtx >>= \ ctx -> return (ctx,s)
  localCtx ctx mf = TurtleT $ \r s -> localCtx ctx (getTurtleT mf r s)