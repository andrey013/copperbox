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
    ZMidiRep(..)
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
  , singleChannel
  , singleTrack
  , vinstrument

  , zmidiRep
  , addTrack
  , track
  ) where

import ZMidi.Emit.GeneralMidiInstruments
import ZMidi.Emit.Utils.JoinList ( JoinList )
import qualified ZMidi.Emit.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Word


-- Note - /destructors/ should destruct to standard datatypes, not
-- JoinLists...
--

data ZMidiRep = ZMidiRep 
      { zm_track_zero   :: Maybe MidiTrack
      , zm_data_tracks  :: JoinList Track 
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
-- Duration is a Double - 0.25 is a quarter note, 1 is a whole 
-- note. It is expected that client software with use some other 
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
newtype VoiceMsg = VoiceMsg { getVoiceMsg :: Word8 -> MidiVoiceEvent }


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

primVoiceMessage :: (Word8 -> MidiVoiceEvent) -> Primitive
primVoiceMessage f = PMsg $ Left $ VoiceMsg f

primMetaEvent :: MidiMetaEvent -> Primitive
primMetaEvent = PMsg . Right

singleChannel :: Int -> ChannelStream -> Track
singleChannel n chan_body = Track $ IM.insert n chan_body IM.empty 


singleTrack :: Track -> ZMidiRep
singleTrack trk = ZMidiRep Nothing $ JL.one trk 


vinstrument :: GMInst -> Primitive
vinstrument inst = 
    primVoiceMessage $ \ch -> ProgramChange ch (instrumentNumber inst)

--------------------------------------------------------------------------------
-- new constructors


zmidiRep :: ZMidiRep
zmidiRep = ZMidiRep Nothing mempty


infixr 5 `addTrack`

addTrack :: ZMidiRep -> Track -> ZMidiRep
addTrack rep trk = rep { zm_data_tracks = jl `JL.snoc` trk}
  where 
    jl = zm_data_tracks rep  


track :: Int -> ChannelStream -> Track
track ch_num ch_body = Track $ IM.insert ch_num ch_body IM.empty 
