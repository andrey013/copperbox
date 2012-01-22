{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.SymObject
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output via a trace (i.e a writer monad).
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.SymObject
  ( 

    SymObject
  , runSymObject
  , symEvent
  , sym1

  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.RenderContext
import ZMidi.Basic.Kernel.Objects.Event

import ZMidi.Basic.Primitive.EventList hiding ( reverse, stretch )
import qualified ZMidi.Basic.Primitive.EventList as Prim
import ZMidi.Basic.Primitive.RenderMidi

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Data.Monoid
import Prelude hiding ( reverse )




newtype SymObject u a = SymObject
         { getSymObject :: RenderContext -> Double -> (a, Double, CatEvent) }

instance Functor (SymObject u) where
  fmap f ma = SymObject $ \ctx s -> let (a,s1,w) = getSymObject ma ctx s
                                    in (f a, s1, w)



instance Applicative (SymObject u) where
  pure a    = SymObject $ \_ s -> (a,s,mempty)
  mf <*> ma = SymObject $ \ctx s -> 
                let (f,s1,w1) = getSymObject mf ctx s
                    (a,s2,w2) = getSymObject ma ctx s1
                in (f a, s2, w1 `mappend` w2)



instance Monad (SymObject u) where
  return    = pure
  ma >>= k  = SymObject $ \ctx s -> 
                let (a,s1,w1) = getSymObject ma ctx s
                    (b,s2,w2) = getSymObject (k a) ctx s1
                in (b, s2, w1 `mappend` w2)


instance Monoid a => Monoid (SymObject u a) where
  mempty          = pure mempty
  ma `mappend` mb = SymObject $ \ctx s -> 
                      let (a,s1,w1) = getSymObject ma ctx s
                          (b,s2,w2) = getSymObject mb ctx s1
                      in (a `mappend` b, s2, w1 `mappend` w2)


 
runSymObject :: InterpretUnit u 
             => RenderContext -> u -> SymObject u a -> (a,u,CatEvent)
runSymObject ctx loc ma = 
    let bpm  = interp_bpm ctx
        dloc = normalize bpm loc
        (a,ddur,w1) = getSymObject ma ctx dloc
        udur        = dinterp bpm ddur
    in (a,udur,w1)


symEvent :: InterpretUnit u => Event u a -> SymObject u a
symEvent ev = SymObject $ \ctx s -> 
                let bpm         = interp_bpm ctx
                    uloc        = dinterp bpm s
                    (a,udur,w1) = runEvent ctx uloc ev
                    ddur        = normalize bpm udur
                in (a, s + ddur, w1)


sym1 :: InterpretUnit u => SymObject u a -> Event u a
sym1 so = promoteLoc $ \ctx u -> runSymObject ctx u so

{-
simul :: (Monoid a, InterpretUnit u) => [Event u a] -> SymObject u a
simul es = SymObject $ \ctx s -> 
                let bpm         = interp_bpm ctx
                    uloc        = dinterp bpm s
                    (a,udur,w1) = runEvent ctx uloc ev
                    ddur        = normalize bpm udur
                in (a, s + ddur, w1)
-}


