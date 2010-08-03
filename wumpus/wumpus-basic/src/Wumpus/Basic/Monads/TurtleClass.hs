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

    TurtleConfig(..)

  , TurtleM(..)

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
 
  ) where


import Wumpus.Core                      -- package: wumpus-core

import Control.Monad

-- Might want to expand this with an initial y-value,
-- otherwise using nextLine will get you negative y-values
-- without care...

data TurtleConfig u = TurtleConfig 
      { xstep :: !u
      , ystep :: !u 
      }  
  deriving (Eq,Show)




class Monad m => TurtleM m u | m -> u where
  getLoc :: m (Point2 u)
  setLoc :: (Point2 u) -> m ()
  xStep  :: m u
  yStep  :: m u


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

