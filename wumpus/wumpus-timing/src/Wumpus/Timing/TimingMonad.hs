{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.TimingMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Monad...
--
--------------------------------------------------------------------------------

module Wumpus.Timing.TimingMonad
  (

    TimingM
  , TimingConfig(..)

  , evalTimingM
  , runTimingM

  , high
  , low

  ) where

import Wumpus.Timing.Core
import Wumpus.Timing.Drawing


import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Monads.TraceMonad
import Wumpus.Basic.Monads.TurtleMonad         

import Data.AffineSpace                 -- package: vector-space
import MonadLib                         -- package: monadLib

import Control.Applicative


data TimingConfig = TimingConfig 
      { signal_height   :: SignalHeight
      , scalefun        :: Int -> Double 
      }

newtype TimingM a = TimingM 
        { getTimingM ::  TurtleT 
                       ( ReaderT TimingConfig 
                       ( TraceT  DPrimitive   Id)) a }


instance Functor TimingM where
  fmap f = TimingM . fmap f . getTimingM

instance Monad TimingM where
  return a = TimingM $ return a
  m >>= k  = TimingM $ getTimingM m >>= getTimingM . k

instance ReaderM TimingM TimingConfig where
  ask = TimingM $ lift $ ask

instance TraceM TimingM DPrimitive where
  trace  h = TimingM $ lift $ lift $ trace h
  trace1 i = TimingM $ lift $ lift $ trace1 i

instance TurtleM TimingM where
  getLoc   = TimingM $ getLoc
  setLoc c = TimingM $ setLoc c

evalTimingM :: TimingConfig -> TimingM a -> DGraphic
evalTimingM cfg mf = snd $ runTimingM cfg mf 

runTimingM :: TimingConfig -> TimingM a -> ((a,(Int,Int)), DGraphic)
runTimingM cfg mf = runId 
                  ( runTraceT
                  ( runReaderT cfg 
                  ( runTurtleT  $ getTimingM mf )))


scaleCoord :: Coord -> TimingM DPoint2 
scaleCoord (Coord x y) = (\sf -> P2 (sf x) (sf y)) <$> asks scalefun



high :: TimingM ()
high = wander moveLeft              >>= \(_,a,b)  -> 
       scaleCoord a                 >>= \start_pt ->
       scaleCoord b                 >>= \end_pt   ->
       trace $ straightLine black (end_pt .-. start_pt) start_pt



low :: TimingM ()
low  = wander moveLeft              >>= \(_,a,b)  -> 
       scaleCoord a                 >>= \start_pt ->
       scaleCoord b                 >>= \end_pt   ->
       trace $ straightLine black (end_pt .-. start_pt) start_pt


