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
  , AudioTrack(..)
  , ChannelStream(..)
  , Section(..)
  , SectionVoice(..)
  , MidiPrim(..)
  , VoiceMsg(..)
  , PrimProps(..)

  , primVoiceMessage
  , primMetaEvent
  , singleChannel
  , singleTrack
  , vinstrument

  ) where

import ZMidi.Emit.GeneralMidiInstruments
import ZMidi.Emit.Utils.JoinList ( JoinList )
import qualified ZMidi.Emit.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Word



newtype ZMidiRep = ZMidiRep { getZMidiRep :: JoinList AudioTrack }
  deriving (Show)

newtype AudioTrack = AudioTrack { getAudioTrack :: IntMap ChannelStream }
  deriving (Show)

newtype ChannelStream = ChannelStream { getSections :: JoinList Section  }
  deriving (Show)


data Section = Section 
      { section_tempo           :: Double
      , section_data            :: JoinList SectionVoice
      }
  deriving (Show)


newtype SectionVoice = SectionVoice { voice_notelist :: [MidiPrim] }
  deriving (Show)


-- | Primitive is either a note, chord or rest.
--
-- Duration is a Double - 0.25 is a quarter note, 1 is a whole 
-- note. It is expected that client software with use some other 
-- type but convert to Double as it builds the syntax.
--
data MidiPrim = PNote   Double PrimProps Word8
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

instance Monoid ZMidiRep where
  mempty        = ZMidiRep mempty
  a `mappend` b = ZMidiRep $ getZMidiRep a `mappend` getZMidiRep b


instance Monoid AudioTrack where
  mempty        = AudioTrack mempty
  a `mappend` b = AudioTrack $ getAudioTrack a `mappend` getAudioTrack b



--------------------------------------------------------------------------------

primVoiceMessage :: (Word8 -> MidiVoiceEvent) -> MidiPrim
primVoiceMessage f = PMsg $ Left $ VoiceMsg f

primMetaEvent :: MidiMetaEvent -> MidiPrim
primMetaEvent = PMsg . Right

singleChannel :: Int -> ChannelStream -> AudioTrack
singleChannel n chan_body = AudioTrack $ IM.insert n chan_body IM.empty 

singleTrack :: AudioTrack -> ZMidiRep
singleTrack trk = ZMidiRep $ JL.one trk 


vinstrument :: GMInst -> MidiPrim
vinstrument inst = 
    primVoiceMessage $ \ch -> ProgramChange ch (instrumentNumber inst)