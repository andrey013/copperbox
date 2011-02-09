{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Extras.Turtle.TurtleMonad
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

module Wumpus.Drawing.Extras.Turtle.TurtleMonad
  (
    -- * Re-exports
    module Wumpus.Drawing.Extras.Turtle.TurtleClass

  -- * Turtle transformer
  , TurtleT
  , runTurtleT

   
  ) where

import Wumpus.Drawing.Extras.Turtle.TurtleClass

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad


-- Note - if Turtle is now just a /local effect/ monad is the 
-- Turtle class still needed? Afterall, there is (probably)
-- only ever going to be one instance.
--

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
          getTurtleT :: ScalingContext Int Int u 
                     -> TurtleState 
                     -> m (a, TurtleState) }

type instance DUnit (TurtleT u m a) = u
    


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
  setLoc c    = TurtleT $ \_ (TurtleState o _)   -> return ((),TurtleState o c)
  getOrigin   = TurtleT $ \_ s@(TurtleState o _) -> return (o,s)
  setOrigin o = TurtleT $ \_ (TurtleState _ c)   -> return ((),TurtleState o c)


runTurtleT :: (Monad m, Num u) 
           => (Int,Int) -> ScalingContext Int Int u -> TurtleT u m a -> m a
runTurtleT ogin cfg mf = getTurtleT mf cfg st0 >>= \(a,_) -> return a
  where 
    st0 = TurtleState ogin ogin 



----------------------------------------------------------------------------------
-- Cross instances

instance DrawingCtxM m => DrawingCtxM (TurtleT u m) where
  askDC           = TurtleT $ \_ s -> askDC >>= \ ctx -> return (ctx,s)
  localize upd mf = TurtleT $ \r s -> localize upd (getTurtleT mf r s)


-- This needs undecidable instances...

instance (Monad m, TraceM m, u ~ DUnit (m ())) => TraceM (TurtleT u m) where
  trace a  = TurtleT $ \_ s -> trace a >> return ((),s)



