{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.TurtleT
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Turtle monad transformer
--
-- Candidate for Wumpus-Extra.
--
--------------------------------------------------------------------------------

module Wumpus.Clave.TurtleT
  (
    Coord(..)

  , TurtleT
  , TurtleM(..)
  , runTurtleT

  , setsLoc
  , setsLoc_

  -- * movement
  , reset
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , nextLine

  , wander
 
  ) where


import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative


data Coord = Coord !Int !Int

instance Show Coord where
  showsPrec i (Coord x y) = showsPrec i (x,y)


newtype TurtleT m a = TurtleT { getTurtleT :: Coord -> m (a, Coord) }

instance Monad m => Functor (TurtleT m) where
  fmap f m = TurtleT $ \st -> getTurtleT m st >>= \(a,st') ->
                              return (f a, st')

instance Monad m => Applicative (TurtleT m) where
  pure a    = TurtleT $ \st -> return (a,st)
  mf <*> ma = TurtleT $ \st -> getTurtleT mf st  >>= \(f,st')  ->
                               getTurtleT ma st' >>= \(a,st'') ->
                               return (f a,st'') 


instance Monad m => Monad (TurtleT m) where
  return a = TurtleT $ \st -> return (a,st)
  m >>= k  = TurtleT $ \st -> getTurtleT m st        >>= \(a,st')  ->
                              (getTurtleT . k) a st' >>= \(b,st'') ->
                              return (b,st'')

instance MonadT TurtleT where
  lift m = TurtleT $ \st -> m >>= \a -> return (a,st)

class Monad m => TurtleM m where
  getLoc :: m Coord
  setLoc :: Coord -> m ()

instance Monad m => TurtleM (TurtleT m) where
  getLoc   = TurtleT $ \st -> return (st,st)
  setLoc c = TurtleT $ \_  -> return ((),c)

runTurtleT :: Monad m => TurtleT m a -> m (a,(Int,Int))
runTurtleT mf = getTurtleT mf (Coord 0 0) >>= \(a, Coord x y) -> return (a,(x,y))


setsLoc :: TurtleM m => (Coord -> (a,Coord)) -> m a
setsLoc f = getLoc >>= \st -> let (a,st') = f st in setLoc st' >> return a

setsLoc_ :: TurtleM m => (Coord -> Coord) -> m ()
setsLoc_ f = getLoc >>= \st -> let st' = f st in setLoc st'

reset       :: TurtleM m => m ()
reset       = setLoc (Coord 0 0)

moveRight   :: TurtleM m => m ()
moveRight   = setsLoc_ $ \(Coord x y) -> Coord (x+1) y

moveLeft    :: TurtleM m => m ()
moveLeft    = setsLoc_ $ \(Coord x y) -> Coord (x-1) y

moveUp      :: TurtleM m => m ()
moveUp      = setsLoc_ $ \(Coord x y) -> Coord x (y-1)

moveDown    :: TurtleM m => m ()
moveDown    = setsLoc_ $ \(Coord x y) -> Coord x (y+1)

nextLine    :: TurtleM m => m ()
nextLine    = setsLoc_ $ \(Coord _ y) -> Coord 0 (y+1)

wander :: TurtleM m => m a -> m (a,Coord,Coord)
wander ma = getLoc >>= \start ->
            ma     >>= \ans   ->
            getLoc >>= \end   ->
            return (ans,start,end)
