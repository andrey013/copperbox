{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
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
-- Turtle monad transformer.
--
-- The Turtle monad embodies the LOGO style of imperative 
-- drawing - sending commands to update the a cursor.
--
-- While Wumpus generally aims for a more compositional,
-- \"coordinate-free\" style of drawing, some types of 
-- diagram are more easily expressed in the LOGO style.
--
-- Turtle is only a transformer - it is intended to be run within
-- a 'Drawing'.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.TurtleMonad
  (


    TurtleT
  , runTurtleT


  , module Wumpus.Basic.Monads.TurtleClass
   
  ) where

import Wumpus.Basic.Monads.TurtleClass



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

newtype TurtleT u m a = TurtleT { 
          getTurtleT :: TurtleConfig u -> TurtleState -> m (a, TurtleState) }


-- Functor


instance Monad m => Functor (TurtleT u m) where
  fmap f m = TurtleT $ \r s -> getTurtleT m r s >>= \(a,s') ->
                               return (f a, s')

-- Applicative 

instance Monad m => Applicative (TurtleT u m) where
  pure a    = TurtleT $ \_ s -> return (a,s)
  mf <*> ma = TurtleT $ \r s -> getTurtleT mf r s  >>= \(f,s')  ->
                                getTurtleT ma r s' >>= \(a,s'') ->
                                return (f a,s'') 


-- Monad 

instance Monad m => Monad (TurtleT u m) where
  return a = TurtleT $ \_ s -> return (a,s)
  m >>= k  = TurtleT $ \r s -> getTurtleT m r s        >>= \(a,s')  ->
                               (getTurtleT . k) a r s' >>= \(b,s'') ->
                               return (b,s'')




instance Monad m => TurtleM (TurtleT u m) where
  getLoc      = TurtleT $ \_ s@(TurtleState _ c) -> return (c,s)
  setLoc c    = TurtleT $ \_   (TurtleState o _) -> return ((),TurtleState o c)
  getOrigin   = TurtleT $ \_ s@(TurtleState o _) -> return (o,s)
  setOrigin o = TurtleT $ \_   (TurtleState _ c) -> return ((),TurtleState o c)

instance Monad m => TurtleScaleM (TurtleT u m) u where
  xStep    = TurtleT $ \r s -> return (xstep r,s)
  yStep    = TurtleT $ \r s -> return (ystep r,s)


-- Run functions discard the state...

runTurtleT :: (Monad m, Num u) 
           => TurtleConfig u -> (Int,Int) -> TurtleT u m a -> m a
runTurtleT cfg ogin mf = liftM fst $ getTurtleT mf cfg (TurtleState ogin ogin)



{-

----------------------------------------------------------------------------------
-- Cross instances

instance DrawingCtxM m => DrawingCtxM (TurtleT u m) where
  askDrawingCtx   = TurtleT $ \_ s -> askDrawingCtx >>= \ ctx -> return (ctx,s)
  localCtx ctx mf = TurtleT $ \r s -> localCtx ctx (getTurtleT mf r s)


-- This needs undecidable instances...

instance (Monad m, TraceM m i) => TraceM (TurtleT u m) i where
  trace a  = TurtleT $ \_ s -> trace a >> return ((),s)


--------------------------------------------------------------------------------

newtype TurtleDrawing u a = TurtleDrawing { 
          getTurtleDrawing :: TurtleT u (Drawing u) a }

instance Functor (TurtleDrawing u) where
  fmap f = TurtleDrawing . fmap f . getTurtleDrawing

-- Applicative 
instance Applicative (TurtleDrawing u) where
  pure a    = TurtleDrawing $ pure a
  mf <*> ma = TurtleDrawing $ getTurtleDrawing mf <*> getTurtleDrawing ma


-- Monad 

instance Monad (TurtleDrawing u) where
  return a = TurtleDrawing $ return a
  m >>= k  = TurtleDrawing $ getTurtleDrawing m >>= (getTurtleDrawing . k)

-- TurtleM

instance TurtleM (TurtleDrawing u) where
  getLoc      = TurtleDrawing $ getLoc 
  setLoc c    = TurtleDrawing $ setLoc c
  getOrigin   = TurtleDrawing $ getOrigin
  setOrigin o = TurtleDrawing $ setOrigin o

instance TurtleScaleM (TurtleDrawing u) u where
  xStep    = TurtleDrawing $ xStep
  yStep    = TurtleDrawing $ yStep


-- Lifters no longer supplied...
-- TraceM 

instance TraceM (TurtleDrawing u) u where
  trace a = TurtleDrawing $ trace a



-- DrawingCtxM

instance DrawingCtxM (TurtleDrawing u) where
  askDrawingCtx   = TurtleDrawing $ askDrawingCtx
  localCtx ctx ma = TurtleDrawing $ localCtx ctx (getTurtleDrawing ma)


runTurtleDrawing :: Num u 
                 => TurtleConfig u 
                 -> (Int,Int)
                 -> DrawingAttr 
                 -> TurtleDrawing u a 
                 -> (a, Graphic u)
runTurtleDrawing cfg ogin attr mf = 
    runDrawing attr ( runTurtleT cfg ogin $ getTurtleDrawing mf)


execTurtleDrawing :: Num u 
                  => TurtleConfig u
                  -> (Int,Int) 
                  -> DrawingAttr 
                  -> TurtleDrawing u a 
                  -> Graphic u
execTurtleDrawing cfg ogin attr mf = 
    snd $ runTurtleDrawing cfg ogin attr mf

-}