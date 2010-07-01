{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Clave.ClaveMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Core types, functions...
--
--------------------------------------------------------------------------------

module Wumpus.Clave.ClaveMonad
  (

    ClaveM
  , ClaveConfig(..)

  , evalClaveM
  , runClaveM

  , beat
  , rest
  , endLine
  , highlight

  ) where

import Wumpus.Clave.Core
import Wumpus.Clave.Drawing
import Wumpus.Clave.TraceT
import Wumpus.Clave.TurtleT

import Wumpus.Core hiding ( trace )     -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import MonadLib                         -- package: monadLib

import Control.Applicative


data ClaveConfig = ClaveConfig 
      { box_height      :: BoxHeight
      , scalefun        :: Int -> Double 
      }

newtype ClaveM a = ClaveM { getClaveM ::  TurtleT 
                                        ( ReaderT ClaveConfig 
                                        ( TraceT  DPrimitive  Id)) a }


instance Functor ClaveM where
  fmap f = ClaveM . fmap f . getClaveM

instance Monad ClaveM where
  return a = ClaveM $ return a
  m >>= k  = ClaveM $ getClaveM m >>= getClaveM . k

instance TraceM ClaveM DPrimitive where
  trace  h = ClaveM $ lift $ lift $ trace h
  trace1 i = ClaveM $ lift $ lift $ trace1 i

instance ReaderM ClaveM ClaveConfig where
  ask      = ClaveM $ lift $ ask

instance TurtleM ClaveM where
  getLoc   = ClaveM $ getLoc
  setLoc c = ClaveM $ setLoc c


evalClaveM :: ClaveConfig -> ClaveM a -> DGraphic
evalClaveM cfg mf = snd $ runClaveM cfg mf 

runClaveM :: ClaveConfig -> ClaveM a -> ((a,(Int,Int)), DGraphic)
runClaveM cfg mf = runId 
                 ( runTraceT
                 ( runReaderT cfg 
                 ( runTurtleT $ getClaveM mf )))


nextCoord :: ClaveM Coord
nextCoord = setsLoc $ \(Coord x y) -> (Coord x y, Coord (x+1) y) 


-- xdistance :: ClaveM Int
-- xdistance = getLoc >>= \(Coord x _) -> return x

scaleCoord :: Coord -> ClaveM DPoint2 
scaleCoord (Coord x y) = (\sf -> P2 (sf x) (sf y)) <$> asks scalefun


beat :: ClaveM ()
beat = nextCoord >>= scaleCoord >>= \pt -> 
       asks box_height          >>= \h  ->
       trace (circleF h black pt)

rest :: ClaveM ()
rest = nextCoord >> return ()

endLine :: ClaveM ()
endLine = getLoc                    >>= \(Coord n y) ->
          asks box_height           >>= \h           ->
          scaleCoord (Coord 0 y)    >>= \pt          ->
          trace (gridF n h 0.5 pt)  >>
          nextLine

-- Doesn't work well with monoid/writer...
 
highlight :: Int -> DRGB -> ClaveM ()
highlight n rgb = getLoc >>= scaleCoord     >>= \pt ->
                  asks box_height           >>= \h  ->
                  trace (backgroundF n h rgb pt)
          