{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.TurtleClass
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

module Wumpus.Basic.Monads.TurtleClass
  (

    Coord
  , TurtleConfig(..)
  , regularConfig 

  , TurtleM(..)
  , TurtleScaleM(..)

  , askSteps
  , setsLoc
  , setsLoc_

  -- * movement
  , resetLoc
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , nextLine
 
  , getPos
  , scaleCoord

  ) where


import Wumpus.Core                      -- package: wumpus-core

import Control.Monad

type Coord = (Int,Int)


-- Might want to expand this with an initial y-value,
-- otherwise using nextLine will get you negative y-values
-- without care...

data TurtleConfig u = TurtleConfig 
      { xstep :: !u
      , ystep :: !u 
      }  
  deriving (Eq,Show)


regularConfig :: u -> TurtleConfig u
regularConfig u = TurtleConfig u u 


class Monad m => TurtleM m where
  getLoc     :: m (Int,Int)
  setLoc     :: (Int,Int) -> m ()
  getOrigin  :: m (Int,Int)
  setOrigin  :: (Int,Int) -> m ()

class TurtleM m => TurtleScaleM m u | m -> u where 
  xStep     :: m u
  yStep     :: m u


askSteps :: TurtleScaleM m u => m (u,u)
askSteps = liftM2 (,) xStep yStep

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


getPos :: (TurtleScaleM m u, Num u) => m (Point2 u)
getPos = getLoc   >>= \(x,y)   ->
         askSteps >>= \(sx,sy) ->
         return $ P2 (sx * fromIntegral x) (sy * fromIntegral y)

scaleCoord  :: (TurtleScaleM m u, Num u) => (Int,Int) -> m (Point2 u)
scaleCoord (x,y) = askSteps >>= \(sx,sy) ->
                   return $ P2 (sx * fromIntegral x) (sy * fromIntegral y)

