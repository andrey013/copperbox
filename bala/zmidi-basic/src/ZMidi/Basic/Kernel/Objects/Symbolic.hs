{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.Symbolic
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic music representation (named pitches, named durations).
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.Symbolic
  ( 
   
   
    note

  -- * Pitch constructors 
  , PitchCons

  , c_nat  
  , c_sharp
  , d_flat
  , d_nat
  , d_sharp
  , e_flat
  , e_nat
  , f_nat
  , f_sharp
  , g_flat
  , g_nat
  , g_sharp
  , a_flat
  , a_nat
  , b_flat
  , b_nat
   
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

note :: MidiPitch -> MidiDuration -> Event MidiDuration ()
note pch drn = onoff (\ch -> NoteOn ch pch 127) 
                     (\ch -> NoteOff ch pch 50) 
                     drn


clampPch :: Int -> MidiPitch
clampPch i | i < 0     = 0
           | i > 127   = 127
           | otherwise = fromIntegral i

-- | A Pitch constructor takes and Octave and builds a MidiPitch.
--
-- Octave is an integer value - middle C is octave 4.
--
-- An octave spans from C to B.
-- 
type PitchCons = Int -> MidiPitch

c_nat       :: PitchCons
c_nat o     = clampPch $     12 * (o + 1)

c_sharp     :: PitchCons
c_sharp o   = clampPch $ 1 + 12 * (o + 1)

d_flat      :: PitchCons
d_flat      = c_sharp

d_nat       :: PitchCons
d_nat o     = clampPch $ 2 + 12 * (o + 1)

d_sharp     :: PitchCons
d_sharp o   = clampPch $ 3 + 12 * (o + 1)

e_flat      :: PitchCons
e_flat      = d_sharp

e_nat       :: PitchCons
e_nat o     = clampPch $ 4 + 12 * (o + 1)

f_nat       :: PitchCons
f_nat o     = clampPch $ 5 + 12 * (o + 1)

f_sharp     :: PitchCons
f_sharp o   = clampPch $ 6 + 12 * (o + 1)

g_flat      :: PitchCons
g_flat      = f_sharp

g_nat       :: PitchCons
g_nat o     = clampPch $ 7 + 12 * (o + 1)

g_sharp     :: PitchCons
g_sharp o   = clampPch $ 8 + 12 * (o + 1)

a_flat      :: PitchCons
a_flat     = a_sharp

a_nat       :: PitchCons
a_nat o     = clampPch $ 9 + 12 * (o + 1)

a_sharp     :: PitchCons
a_sharp o   = clampPch $ 10 + 12 * (o + 1)

b_flat      :: PitchCons
b_flat      = a_sharp

b_nat       :: PitchCons
b_nat o     = clampPch $ 11 + 12 * (o + 1)



-- | A whole beat.
--
-- Value @1.0@.
--
dwhole      :: MidiDuration
dwhole      = 1.0

-- | A half beat.
-- 
-- Value @0.5@.
--
dhalf       :: MidiDuration
dhalf       = 0.5

-- | A quarter beat.
-- 
-- Value @0.25@.
--
dquarter    :: MidiDuration
dquarter    = 0.25

-- | A eighth beat.
-- 
-- Value @0.125@.
--
deighth     :: MidiDuration
deighth     = 0.125


-- | A sixteenth beat.
-- 
-- Value @0.0625@.
--
dsixteenth  :: MidiDuration
dsixteenth   = 0.0625

