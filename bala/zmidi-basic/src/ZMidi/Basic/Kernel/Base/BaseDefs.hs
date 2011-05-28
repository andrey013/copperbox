{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Base.BaseDefs
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Base.BaseDefs
  ( 
    
    Durational 

  , ticks_per_quarternote
  , durationDT

  ) where

import ZMidi.Core

-- | Some unit of duration usually Double for MIDI (rendered to 
-- Word32).
--
-- This very useful for reducing the kind of type classes to *.
-- 
-- Then constraints on the Unit type can be declared on the 
-- instances rather than in the class declaration.
-- 
type family Durational a :: *


--------------------------------------------------------------------------------

-- Note - this is a Double so it is the right type for calculating
-- durations, in MIDI files the size of the ticks-per-beat 
-- designator is actually a Word16.
--
ticks_per_quarternote :: Double
ticks_per_quarternote = 480



durationDT :: Double -> DeltaTime
durationDT r = floor $ (4 * ticks_per_quarternote) * r

