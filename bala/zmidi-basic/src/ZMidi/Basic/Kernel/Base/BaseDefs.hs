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
    
    DUnit

  , OnsetTime
  , BPM
  , GMInst
  , GMDrum

  , MidiPitch
  , MidiDuration

  , InterpretUnit(..)
  , uconvert

  ) where


import Data.Word

-- | Some unit of duration usually Double for MIDI (rendered to 
-- Word32).
--
-- This very useful for reducing the kind of type classes to *.
-- 
-- Then constraints on the Unit type can be declared on the 
-- instances rather than in the class declaration.
--
-- (Cf. @DUnit@ in Wumpus)
-- 
type family DUnit a :: *



-- | Internally represent Onset times (and durations) as Double.
-- 
-- Only in a final conversion step are ticks (MIDI\'s 
-- representation)use.
-- 
type OnsetTime = Double


-- | Beats per minute - just an alias to Double.
--
type BPM = Double


-- | Enumeration of the General MIDI instruments.
--
type GMInst = Word8



-- | Enumeration of the General MIDI drum types.
--
type GMDrum = Word8

-- | 'MidiPitch' is a Word8, corresponding directly to the MIDI
-- representation, only values in the range @[0..127]@ are
-- allowed.
--
-- Middle c is 60. Each increment is a semitone step. With careful
-- use of the pitch-bend control signal MIDI can simulate
-- microtonal intervals though this is not attempted by
-- @ZMidi.Emit@.
--
type MidiPitch    = Word8


-- | 'MidiDuration' is a Double, directly corresponding to the
-- duration value:
--
-- @1.0@ represents a whole note.
--
-- @0.5@ represents a half note.
--
-- @0.25@ represents a half note. Etc.
--
-- Using Double allows some cleverness for representing special
-- durations, e.g. grace notes can be some small duration
-- subtracted from the note next to the grace.
--
-- Internally @ZMidi.Emit@ translates the Double value into an
-- integer number of ticks.
--
type MidiDuration = Double

-- TODO - newtype wrap...


class Num u => InterpretUnit u where
  normalize  :: BPM -> u -> Double
  dinterp    :: BPM -> Double -> u


instance InterpretUnit Double where
  normalize _ = id
  dinterp _   = id



-- | Convert a scalar value from one unit to another.
--
uconvert :: (InterpretUnit u, InterpretUnit u1) => TicksSize -> u -> u1
uconvert sz = dinterp sz . normalize sz


--------------------------------------------------------------------------------

type TicksSize = Double




