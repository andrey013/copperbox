{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
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

    TurtleConfig(..)

  , Turtle
  , TurtleT

  , TurtleM(..)

  , runTurtle
  , runTurtleT

  , evalTurtle
  , evalTurtleT 

  , setsLoc
  , setsLoc_



  -- * movement
  , resetLoc
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , nextLine
 
  ) where


import Wumpus.Core                      -- package: wumpus-core

import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative
import Control.Monad

-- Might want to expand this with an initial y-value,
-- otherwise using nextLine will get you negative y-values
-- without care...

data TurtleConfig u = TurtleConfig 
      { xstep :: !u
      , ystep :: !u 
      }  
  deriving (Eq,Show)


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


class Monad m => TurtleM m u | m -> u where
  getLoc :: m (Point2 u)
  setLoc :: (Point2 u) -> m ()
  xStep  :: m u
  yStep  :: m u

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


askSteps :: TurtleM m u => m (u,u)
askSteps = liftM2 (,) xStep yStep

setsLoc :: TurtleM m u 
        => ((u,u) -> Point2 u -> (a,Point2 u)) -> m a
setsLoc f = getLoc      >>= \pt -> 
            askSteps    >>= \sc ->
            let (a,pt') = f sc pt in setLoc pt' >> return a

setsLoc_ :: TurtleM m u => ((u,u) -> Point2 u -> Point2 u) -> m ()
setsLoc_ f = getLoc     >>= \pt -> 
             askSteps   >>= \sc ->
             let pt' = f sc pt in setLoc pt'



resetLoc    :: (TurtleM m u, Num u) => m ()
resetLoc    = setLoc (P2 0 0)


moveRight   :: (TurtleM m u, Num u) => m ()
moveRight   = setsLoc_ $ \(xi,_) (P2 x y) -> P2 (x+xi) y


moveLeft    :: (TurtleM m u, Num u) => m ()
moveLeft    = setsLoc_ $ \(xi,_) (P2 x y) -> P2 (x-xi) y

moveUp      :: (TurtleM m u, Num u) => m ()
moveUp      = setsLoc_ $ \(_,yi) (P2 x y) -> P2 x (y+yi)

moveDown    :: (TurtleM m u, Num u) => m ()
moveDown    = setsLoc_ $ \(_,yi) (P2 x y) -> P2 x (y-yi)


-- Note this will draw things with negative y-coorinates unless
-- you intially seed the state with a high x-pos ...

nextLine    :: (TurtleM m u , Num u) => m ()
nextLine    = setsLoc_ $ \(_,yi)(P2 _ y) -> P2 0 (y-yi)

