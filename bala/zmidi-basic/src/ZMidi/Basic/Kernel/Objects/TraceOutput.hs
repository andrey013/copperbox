{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.TraceOutput
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output via a trace (i.e a writer monad).
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.TraceOutput
  ( 
    trace
  
  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.RenderContext
import ZMidi.Basic.Kernel.Objects.Event

import ZMidi.Basic.Primitive.RenderMidi

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid

trace :: InterpretUnit u => RenderContext -> [(u,Event u a)] -> MidiFile
trace ctx = genFormat1 . step mempty
  where
    step ac ((u,f):xs) = let (_,_,w1) = runEvent ctx u f
                         in step (ac `mappend` w1) xs
    step ac []         = ac
                          
