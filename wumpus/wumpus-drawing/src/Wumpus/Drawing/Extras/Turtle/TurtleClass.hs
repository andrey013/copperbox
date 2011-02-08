{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Extras.Turtle.TurtleClass
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
-- \"coordinate-free\" style of drawing, some types of diagram 
-- are more easily expressed in the LOGO style.
--
-- Note - as turtle drawing with Wumpus is a /local effect/, 
-- there is only one instance of TurtleM. Potentially TurtleM 
-- will be removed and the functions implemented directly. 
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Extras.Turtle.TurtleClass
  (

    Coord

  , TurtleM(..)

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



type Coord = (Int,Int)


class Monad m => TurtleM m where
  getLoc     :: m (Int,Int)
  setLoc     :: (Int,Int) -> m ()
  getOrigin  :: m (Int,Int)
  setOrigin  :: (Int,Int) -> m ()



setsLoc :: TurtleM m => (Coord -> (a,Coord)) -> m a
setsLoc f = getLoc      >>= \coord -> 
            let (a,coord') = f coord in setLoc coord' >> return a

setsLoc_ :: TurtleM m => (Coord -> Coord) -> m ()
setsLoc_ f = getLoc     >>= \coord ->  setLoc (f coord)


resetLoc    :: TurtleM m => m ()
resetLoc    = getOrigin >>= setLoc


moveRight   :: TurtleM m => m ()
moveRight   = setsLoc_ $ \(x,y)-> (x+1, y)


moveLeft    :: TurtleM m => m ()
moveLeft    = setsLoc_ $ \(x,y) -> (x-1,y)

moveUp      :: TurtleM m => m ()
moveUp      = setsLoc_ $ \(x,y) -> (x,y+1)

moveDown    :: TurtleM m => m ()
moveDown    = setsLoc_ $ \(x,y) -> (x ,y-1)


nextLine    :: TurtleM m => m ()
nextLine    = getOrigin >>= \(ox,_) ->
              setsLoc_ $ \(_,y) -> (ox,y-1)

