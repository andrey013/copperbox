{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.Symbolic
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC (GeneralizedNewtypeDeriving...)
--
-- Symbolic music representation (named pitches, named durations).
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.Symbolic
  ( 
   
   
    note
  , rest
  , chord
  , MidiDuration
  , MidiPitch
  , PitchClass(..)
  , midiPitch

   
   -- * Duration values
  , dwhole
  , dhalf
  , dquarter
  , deighth
  , dsixteenth


  ) where


import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Objects.Event

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid
import Data.Word

note :: PitchClass -> Int -> MidiDuration -> Event MidiDuration ()
note pc o drn = let p = midiPitch pc o 
                in onoff (\ch -> NoteOn ch p 127) 
                         (\ch -> NoteOff ch p 50) 
                         drn

rest :: MidiDuration -> Event MidiDuration ()
rest = blank


chord :: [(PitchClass,Int)] -> MidiDuration -> Event MidiDuration ()
chord xs d = mconcat $ map fn xs 
  where
    fn (pc,o) = note pc o d


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
-- @0.25@ represents a quarter note. Etc.
--
-- Using Double allows some cleverness for representing special
-- durations, e.g. grace notes can be some small duration
-- subtracted from the note next to the grace.
--
-- Internally @ZMidi.Emit@ translates the Double value into an
-- integer number of ticks.
--
newtype MidiDuration = MidiDuration { getMidiDuration :: Double }
  deriving (Eq,Ord,Num,Fractional) 

instance Show MidiDuration where
  showsPrec p i = showsPrec p (getMidiDuration i)

-- bpm is number of quarter notes in a minute
-- 120bpm - each quarter note is 0.5 seconds long


instance InterpretUnit MidiDuration where
  normalize bpm (MidiDuration d) = d  / (bpm / 60)
  dinterp bpm secs = MidiDuration $ secs * (bpm / 60) 



data PitchClass = Cf | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F
                | Fs | Gf | G | Gs | Af | A | As | Bf | B | Bs
  deriving (Bounded,Enum,Eq,Ord,Show)

pitchClass :: PitchClass -> Int
pitchClass Cf = (-1)
pitchClass C  = 0
pitchClass Cs = 1
pitchClass Df = 1
pitchClass D  = 2
pitchClass Ds = 3
pitchClass Ef = 3
pitchClass E  = 4
pitchClass Es = 5
pitchClass Ff = 4  -- Ff = E
pitchClass F  = 5
pitchClass Fs = 6
pitchClass Gf = 6
pitchClass G  = 7
pitchClass Gs = 8
pitchClass Af = 8
pitchClass A  = 9
pitchClass As = 10
pitchClass Bf = 10
pitchClass B  = 11
pitchClass Bs = 12

-- | 'midiPitch' takes a PicthClass and an Octave and builds a 
-- MidiPitch.
--
-- Octave is an integer value - middle C is octave 4.
--
-- An octave spans from C to B.
-- 
midiPitch :: PitchClass -> Int -> MidiPitch
midiPitch pc o = clampPitch $ (12 * (o+1)) + pitchClass pc



clampPitch :: Int -> MidiPitch
clampPitch i | i < 0     = 0
             | i > 127   = 127
             | otherwise = fromIntegral i


-- | A whole note - 4 beats.
--
-- Value @4.0@.
--
dwhole      :: MidiDuration
dwhole      = 4.0

-- | A half note - 2 beats.
-- 
-- Value @2.0@.
--
dhalf       :: MidiDuration
dhalf       = 2.0

-- | A quarter note - 1 beat.
-- 
-- Value @1.0@.
--
dquarter    :: MidiDuration
dquarter    = 1.0

-- | A eighth note - 1/2 a beat.
-- 
-- Value @0.5@.
--
deighth     :: MidiDuration
deighth     = 0.5


-- | A sixteenth note - 1/4 a beat.
-- 
-- Value @0.25@.
--
dsixteenth  :: MidiDuration
dsixteenth   = 0.25

