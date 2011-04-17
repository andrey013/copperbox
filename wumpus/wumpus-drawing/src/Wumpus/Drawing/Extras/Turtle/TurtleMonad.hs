{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
          getTurtleT :: TurtleState -> m (a, TurtleState) }

    
type instance MonUnit (TurtleT u m a) = u

-- Functor



instance Monad m => Functor (TurtleT u m) where
  fmap f m = TurtleT $ \s -> getTurtleT m s >>= \(a,s') ->
                             return (f a, s')


-- Applicative 

instance Monad m => Applicative (TurtleT u m) where
  pure a    = TurtleT $ \s -> return (a,s)
  mf <*> ma = TurtleT $ \s -> getTurtleT mf s  >>= \(f,s')  ->
                              getTurtleT ma s' >>= \(a,s'') ->
                              return (f a,s'') 


-- Monad 

instance Monad m => Monad (TurtleT u m) where
  return a = TurtleT $ \s -> return (a,s)
  m >>= k  = TurtleT $ \s -> getTurtleT m s   >>= \(a,s')  ->
                             (getTurtleT . k) a s'




instance Monad m => TurtleM (TurtleT u m) where
  getLoc      = TurtleT $ \s@(TurtleState _ c) -> return (c,s)
  setLoc c    = TurtleT $ \(TurtleState o _)   -> return ((),TurtleState o c)
  getOrigin   = TurtleT $ \s@(TurtleState o _) -> return (o,s)
  setOrigin o = TurtleT $ \(TurtleState _ c)   -> return ((),TurtleState o c)


runTurtleT :: (Monad m, Num u) 
           => (Int,Int) -> TurtleT u m a -> m a
runTurtleT ogin mf = getTurtleT mf st0 >>= \(a,_) -> return a
  where 
    st0 = TurtleState ogin ogin 



----------------------------------------------------------------------------------
-- Cross instances

instance DrawingCtxM m => DrawingCtxM (TurtleT u m) where
  askDC           = TurtleT $ \s -> askDC >>= \ ctx -> return (ctx,s)
  localize upd mf = TurtleT $ \s -> localize upd (getTurtleT mf s)


-- This needs undecidable instances...

instance (Monad m, TraceM m, u ~ MonUnit (m ()) ) => TraceM (TurtleT u m) where
  trace a      = TurtleT $ \s -> trace a >> return ((),s)
  fontDelta mf = TurtleT $ \s -> fontDelta $ getTurtleT mf s


