{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Emit.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
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

module ZMidi.Emit.Datatypes
  ( 
    HiMidi(..)
  , Track(..)
  , ChannelStream(..)
  , Section(..)
  , Overlays
  , SectionVoice(..)
  , Primitive(..)
  , VoiceMsg(..)
  , PrimProps(..)

  , primVoiceMessage
  , primMetaEvent

  , hiMidi
  , addTrack
  , track

  ) where

import ZMidi.Emit.Utils.JoinList ( JoinList )
import qualified ZMidi.Emit.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Word


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
--
data HiMidi = HiMidi
      { hm_track_zero   :: Maybe MidiTrack
      , hm_data_tracks  :: JoinList Track 
      }
  deriving (Show)


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
newtype Track = Track { getTrack :: IntMap ChannelStream }
  deriving (Show)


-- | Channel number is really a @Word4@ so the value should be in 
-- the range [0..15].
--
-- Channel 9 is special - this is the channel for MIDI percussion.
--
type ChannelNumber = Word8


-- | ChannelStream supports concatenation through @mappend@.
-- Concantenation is sequential - the notes from the second 
-- ChannelStream are concatenated after the notes in the first 
-- stream to for the amalgamated stream.
--
newtype ChannelStream = ChannelStream { getSections :: JoinList Section  }
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
      , section_overlays        :: Overlays
      }
  deriving (Show)


type Overlays = JoinList SectionVoice


-- | A section voice allows chords, but otherwise it is 
-- monophonic.
--
newtype SectionVoice = SectionVoice { voice_notelist :: [Primitive] }
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
      , note_volume     :: Word8
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

instance Monoid ChannelStream where
  mempty        = ChannelStream mempty
  a `mappend` b = ChannelStream $ getSections a `mappend` getSections b



--------------------------------------------------------------------------------

primVoiceMessage :: (ChannelNumber -> MidiVoiceEvent) -> Primitive
primVoiceMessage f = PMsg $ Left $ VoiceMsg f

primMetaEvent :: MidiMetaEvent -> Primitive
primMetaEvent = PMsg . Right


--------------------------------------------------------------------------------
-- new constructors


hiMidi :: HiMidi
hiMidi = HiMidi Nothing mempty


infixr 5 `addTrack`

addTrack :: HiMidi -> Track -> HiMidi
addTrack rep trk = rep { hm_data_tracks = jl `JL.snoc` trk}
  where 
    jl = hm_data_tracks rep  


track :: Int -> ChannelStream -> Track
track ch_num ch_body = Track $ IM.insert ch_num ch_body IM.empty 
