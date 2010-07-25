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

  , evalTimingM
  , runTimingM

  , high
  , low
  , highImp
  , undef

  , glitch

  ) where

import Wumpus.Timing.Alphabet
import Wumpus.Timing.STraceMonad

import Wumpus.Basic.Utils.HList         -- package: wumpus-basic


import MonadLib                         -- package: monadLib

import Control.Applicative


type TimingState = Maybe Letter

newtype TimingM a = TimingM 
        { getTimingM ::  StateT  TimingState 
                       ( STraceT Letter      Id) a }


instance Functor TimingM where
  fmap f = TimingM . fmap f . getTimingM

instance Applicative TimingM where
  pure a = TimingM $ return a
  f <*> a = f `ap` a


instance Monad TimingM where
  return a = TimingM $ return a
  m >>= k  = TimingM $ getTimingM m >>= getTimingM . k



instance STraceM TimingM Letter where
  strace  h = TimingM $ lift $ strace h
  strace1 i = TimingM $ lift $ strace1 i

instance StateM TimingM TimingState where
  get   = TimingM $ get
  set s = TimingM $ set s

setL :: Letter -> TimingM ()
setL = set . Just

evalTimingM :: TimingM a -> [Letter]
evalTimingM mf = toListH $ snd $ runTimingM mf 

runTimingM :: TimingM a -> (a, H Letter)
runTimingM mf = post $ runId 
                     ( runSTraceT
                     ( runStateT Nothing $ getTimingM mf ))
  where
    post ((a,Nothing),hf)  = (a,hf)
    post ((a,Just ltr),hf) = (a,hf `snocH` ltr)


high :: TimingM ()
high = get >>= fn
  where
    fn (Just (H w)) = setL $ H (w+1)
    fn (Just ltr)   = setL (H 1) >> strace1 ltr
    fn Nothing      = setL (H 1)

low :: TimingM ()
low = get >>= fn
  where
    fn (Just (L w)) = setL $ L (w+1)
    fn (Just ltr)   = setL (L 1) >> strace1 ltr
    fn Nothing      = setL (L 1)

highImp :: TimingM ()
highImp = get >>= fn
  where
    fn (Just (Z w)) = setL $ Z (w+1)
    fn (Just ltr)   = setL (Z 1) >> strace1 ltr
    fn Nothing      = setL (Z 1)


undef :: TimingM ()
undef = get >>= fn
  where
    fn (Just (X w)) = setL $ X (w+1)
    fn (Just ltr)   = setL (X 1) >> strace1 ltr
    fn Nothing      = setL (X 1)

glitch :: TimingM ()
glitch = get >>= fn
  where
    fn (Just G)     = return () 
    fn (Just ltr)   = setL G >> strace1 ltr
    fn Nothing      = setL G 
