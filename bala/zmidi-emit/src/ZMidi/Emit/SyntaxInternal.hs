{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.SyntaxInternal
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Syntax for /high level/ MIDI.
--
--
--------------------------------------------------------------------------------

module ZMidi.Emit.SyntaxInternal
  ( 

  -- * Type synonyms
    MidiPitch
  , MidiDuration
  , GMInst
  , GMDrum

  -- * Higher level syntax
  , HiMidi(..)
  , Track(..)
  , Voice(..)
  , Section(..)
  , Overlay(..)
  , Primitive(..)
  , VoiceMsg(..)
  , PrimProps(..)

  , primVoiceMessage
  , primMetaEvent


  ) where

import ZMidi.Emit.Utils.JoinList ( JoinList )

import ZMidi.Core                               -- package: zmidi-core

import Data.IntMap ( IntMap )
import Data.Monoid
import Data.Word




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
-- dureation value:
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
-- Internally @ZMidi.Emit@ translates the Double value into and
-- integer number of ticks.
--
type MidiDuration = Double

-- | Enumeration of the General MIDI instruments.
--
type GMInst = Word8



-- | Enumeration of the General MIDI drum types.
--
type GMDrum = Word8




--------------------------------------------------------------------------------
-- High level syntax

-- | High-level representation of Format 1 MIDI files.
--
-- Notes, chords and rests represented directly (unlike MIDI 
-- which represents notes and chords as synthesizer key-press and 
-- key-release events).
--
-- As per Format 1 MIDI, the HiMidi representation allows 
-- simultaneous overlaid tracks. Within a track sections (possibly
-- with changing tempos or different instruments) can be 
-- sequentially concatenated.
--
-- HiMidi also contains an info track. Some text events (e.g. 
-- copyright notice, lyrics) can be added to the info track, by 
-- default if the info track is empty @ZMidi-Emit@ will write a
-- timestamp in the output file.
--  
--
data HiMidi = HiMidi
      { hm_info_track   :: JoinList MidiMetaEvent
      , hm_data_tracks  :: JoinList Track 
      }
  deriving (Show)

--
-- Note HiMidi is probably superfluous.
--
-- The only \"interesting\" operation is write:
--
-- > writeHiMidi :: FilePath -> [MidiMetaEvent] -> [Track] -> IO ()
--
-- 


-- | A Track supports upto 16 channels of simultaneous /voices/.
--
-- Channels must be indexed in the range @[0..15]@, indexes 
-- outside this range will be dropped when the track is rendered.
-- 
-- Tracks support superimposition as @mappend@, note that 
-- mappend is left biased - if both tracks have a stream on 
-- channel 1, mappend will take the stream from the left track 
-- and discard the stream from the right track. This behaviours
-- follows the behaviour of @Data.IntMap@, internally 'Track'
-- is represented by and @IntMap@.
--
-- 
-- Note - channel 9 is reserved for MIDI percussion.
--
newtype Track = Track { getTrack :: IntMap Voice }
  deriving (Show)


-- | Channel number is really a @Word4@ so the value should be in 
-- the range [0..15].
--
-- Channel 9 is special - this is the channel for MIDI percussion.
--
type ChannelNumber = Word8


-- | Voice supports concatenation through @mappend@.
--
-- Concantenation is sequential - the notes from the second 
-- Voice are concatenated after the notes in the first.
--
newtype Voice = Voice { getVoice :: JoinList Section  }
  deriving (Show)


-- | Section allows overlayed voices - so this organisation makes
-- it possible to have /resource contention/ (simultaneous 
-- requests for same note on same channel).
--
-- But having overlays makes efficient use of a channel if we 
-- assume there wont be resource contention (e.g. for bass plus
-- treble).
--
data Section = Section 
      { section_tempo           :: Double
      , section_overlays        :: JoinList Overlay
      }
  deriving (Show)




-- | A section voice allows chords, but otherwise it is 
-- monophonic.
--
newtype Overlay = Overlay { getOverlay :: [Primitive] }
  deriving (Show)


-- | Primitive is either a note, chord or rest.
--
-- Pitch is a Word8 - the MIDI pitch number. It is expected that
-- client software will use some other type and call the pitch
-- constructors in @ZMidi.Emit.Builder@ to build the syntax.
--
-- Duration is a Double - 0.25 is a quarter note, 1 is a whole 
-- note. It is expected that client software will use some other 
-- type but convert to Double as it builds the syntax.
--
data Primitive = PNote   Double PrimProps Word8
               | PChord  Double PrimProps [Word8]
               | PRest   Double
               | PMsg    (Either VoiceMsg MidiMetaEvent)
   deriving (Show)



-- | 'VoiceMsg' is a function from channel number to VoiceEvent.
-- 
-- Channel number is unknown when building the syntax, it is 
-- filled in during rendering.
--
newtype VoiceMsg = VoiceMsg { getVoiceMsg :: ChannelNumber -> MidiVoiceEvent }


instance Show VoiceMsg where
  show _ = "VoiceMsg <function>"


-- All notes in a chord have the same properties...
--
data PrimProps = PrimProps
      { velocity_on     :: Word8
      , velocity_off    :: Word8
      , prim_volume     :: Word16
      , prim_balance    :: Word16  -- 00 hard left, 7F7F hard right
      , prim_inst       :: Word8
      }
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- instances

-- Monoid



-- This follows the Monoid of IntMap which is left biased.
--
instance Monoid Track where
  mempty        = Track mempty
  a `mappend` b = Track $ getTrack a `mappend` getTrack b

instance Monoid Voice where
  mempty        = Voice mempty
  a `mappend` b = Voice $ getVoice a `mappend` getVoice b



--------------------------------------------------------------------------------

primVoiceMessage :: (ChannelNumber -> MidiVoiceEvent) -> Primitive
primVoiceMessage f = PMsg $ Left $ VoiceMsg f

primMetaEvent :: MidiMetaEvent -> Primitive
primMetaEvent = PMsg . Right


