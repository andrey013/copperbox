{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.Construction
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Construction for \"syntax\", pitches, durations, General MIDI 
-- instruments and General MIDI Drums.
--
--------------------------------------------------------------------------------

module ZMidi.Emit.Construction
  (


  -- * \"Syntax\" construction
    hiMidi
  , addTrack
  , addT
  , track

  , hplus

  -- ** Meta info
  , MetaInfo            -- opaque
  , meta
  , copyrightNotice
  , lyrics
  , genericText


  -- ** Monadically build note lists  
  , NoteList            -- re-export

  , monoVoice
  , polyVoice

  , instrument
  , volume
  , note
  , chord
  , rest

  , EnvTransformer
  , localize            -- re-export
  , noteOnVelo  
  , noteOffVelo  

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

  -- * Duration constructors
  , dwhole
  , dhalf
  , dquarter
  , deighth
  , dsixteenth

  ) where

import ZMidi.Emit.SyntaxInternal
import ZMidi.Emit.Builder
import ZMidi.Emit.Utils.InstrumentName
import qualified ZMidi.Emit.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Data.Bits
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Word



--------------------------------------------------------------------------------
-- new constructors

-- | Build an initial HiMidi object. 
-- 
-- HiMidi is built in a /snoc list/ style, to add tracks use 
-- 'addTrack':
--
-- > demo_hi_midi :: HiMidi 
-- > demo_hi_midi = hiMidi `addTrack` percussion_track `addTrack` piano_track
-- >   where
-- >     percussion_track = ...
-- >     piano_track      = ...
-- >
--
hiMidi :: HiMidi
hiMidi = HiMidi mempty mempty


infixl 5 `addTrack`, `addT`

-- | 'addTrack' : @ hi_midi_object * track -> HiMidi @ 
--
-- Add a track to a HiMidi object. 
-- 
-- Note - that the argument order does not follow the usual 
-- Haskell convention (cf. @insert@ on Data.Map for example). This 
-- is because the internal representation allows efficient adding 
-- to the right tip as per a /snoc list/.
-- 
addTrack :: HiMidi -> Track -> HiMidi
addTrack rep trk = rep { hm_data_tracks = jl `JL.snoc` trk}
  where 
    jl = hm_data_tracks rep  


-- | 'addT' a synonym for 'addTrack'.
--
-- This might be preferred to 'addTrack' as it is four letters
-- long like 'meta' and so may vertically align better. 
-- 
addT :: HiMidi -> Track -> HiMidi
addT = addTrack


-- | 'track' : @ chan_num * voice -> Track @
-- 
-- Create a 'Track' from a 'Voice', assigning it
-- to the channel @chan_num@.
--
track :: Int -> Voice -> Track
track ch_num ch_body = Track $ IM.insert ch_num ch_body IM.empty 


infixl 5 `hplus`

hplus :: Voice -> Voice -> Voice
hplus = mappend

{-
-- vplus feels the wrong name - or Track is the wrong name.
-- Really we are adding channels (maps of channels).
--
vplus :: Track -> Track -> Track
vplus = mappend
-}


--------------------------------------------------------------------------------
-- Text events 


-- | An opaque type representing information that can be added
-- to an /info track/ in the MIDI output. 
-- 
newtype MetaInfo = MetaInfo { getMetaInfo :: MidiMetaEvent }

infixl 6 `meta`

-- | Add 'MetaInfo' (e.g. copyright notice) to the HiMidi object.
-- 
-- As per 'addTrack' the argument order here follows the 
-- /snoc list/ convention rather than the usual Haskell 
-- convention.
-- 
-- THIS IS SLOW (pending replacement...)
-- 
meta :: HiMidi -> MetaInfo -> HiMidi
meta rep meta_info = rep { hm_info_track = info }
  where
    info = hm_info_track rep `JL.snoc` getMetaInfo meta_info



-- | Copyright notice meta information.
-- 
copyrightNotice :: String -> MetaInfo
copyrightNotice msg = MetaInfo $ TextEvent COPYRIGHT_NOTICE msg

-- | Lyrics meta information.
-- 
lyrics :: String -> MetaInfo
lyrics msg = MetaInfo $ TextEvent LYRICS msg

-- | Generic text meta information.
-- 
genericText :: String -> MetaInfo
genericText msg = MetaInfo $ TextEvent GENERIC_TEXT msg



--------------------------------------------------------------------------------
-- Monadic building


-- | Build a 'SectionVoice' from a 'NoteList'.
--
-- Effectively this is a @run@ function for the 'NoteList' monad.
--
overlay :: NoteList a -> Overlay
overlay = Overlay . execNoteList build_env_zero



-- | 'monoVoice' : @ bpm * note_list -> Voice @
--
-- Build a monophonic 'Voice' from a 'NoteList' played at the 
-- supplied tempo (bpm).
--
-- Effectively this is a @run@ function for the 'NoteList' monad.
--
monoVoice :: Double -> NoteList a -> Voice
monoVoice bpm xs = Voice $ JL.one $ Section bpm $ JL.one $ overlay xs


-- | 'overlays' : @ bpm * [note_list] -> Voice @
--
-- Build a polyphonic 'Voice' by simultaneously overlaying the 
-- NoteLists. All the overlayed NoteLists are played at the 
-- supplied tempo (bpm).
--
-- Effectively this is a @run@ function for the 'NoteList' monad.
--
polyVoice :: Double -> [NoteList a] -> Voice
polyVoice bpm xs = 
    Voice $ JL.one $ Section bpm $ JL.fromListF overlay xs





-- | Add an instrument change to the note list - all subsequent
-- notes on this channel will be played with this instrument.
--
instrument :: GMInst -> NoteList ()
instrument inst  = report prog >> report name
  where
    prog = primVoiceMessage $ \ch -> ProgramChange ch inst
    name = primMetaEvent $ TextEvent INSTRUMENT_NAME (instrumentName inst)    


-- | Change the channel volume level - all subsequent
-- notes on this channel will be played at this volume.
--
-- Note - volume level is a 14-bit value rather than a 16-bit 
-- (Word16) value, thus the max value is 16383.
--
volume :: Word16 -> NoteList ()
volume vol  = report ctrl7 >> report ctrl38
  where
    lsb     = fromIntegral $ vol .&. 0x7F
    msb     = fromIntegral $ (vol `shiftR` 7) .&. 0x7F
    ctrl7   = primVoiceMessage $ \ch -> Controller ch 7  msb
    ctrl38  = primVoiceMessage $ \ch -> Controller ch 38 lsb



-- | Add a note to the note list.
--
note :: MidiPitch -> MidiDuration -> NoteList ()
note p d = noteProps >>= \props -> report (PNote d props p)


-- | Add a chord to the note list.
--
-- Note - all the pitches should be different. @ZMidi-Emit@ 
-- transmits all chord notes to MIDI as note-on, note-off pairs, 
-- so duplicated notes will cause contention for the (virtual) 
-- keyboard key.
-- 
chord :: [MidiPitch] -> MidiDuration -> NoteList ()
chord ps d = noteProps >>= \props -> report (PChord d props ps)

-- | Add a rest to the note list.
--
rest :: MidiDuration -> NoteList ()
rest d = report $ PRest d





-- | Set the note-on velocity in the NoteList environment.
--
noteOnVelo      :: Word8 -> EnvTransformer
noteOnVelo v    = \s -> s { note_on_velocity = v}


-- | Set the note-off velocity in the NoteList environment.
--
noteOffVelo      :: Word8 -> EnvTransformer
noteOffVelo v    = \s -> s { note_off_velocity = v}



--------------------------------------------------------------------------------

clampPch :: Int -> Word8
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

