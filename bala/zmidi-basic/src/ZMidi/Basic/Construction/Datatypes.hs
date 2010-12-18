{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Construction.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Syntax for /high level/ MIDI.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Construction.Datatypes
  ( 
    MultiChannelTrack
  , Section(..)
  , SectionVoice(..)
  , MidiPrim(..)
  , VoiceMsg(..)
  , PrimProps(..)

  , vinstrument

  ) where

import ZMidi.Basic.Construction.JoinList

import ZMidi.Core                               -- package: zmidi-core

import Data.Word


-- Note MultiChannel track currently isn\'t multi (it should be to 
-- support playing different rhythms against each other). 

type MultiChannelTrack = JoinList ChannelTrack

type ChannelTrack = JoinList Section

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
              | PMsg    (Either VoiceMsg MetaEvent)
   deriving (Show)

-- | 'VoiceMsg' is a function from channel number to VoiceEvent.
-- 
-- Channel number is unknown when building the syntax, it is 
-- filled in during rendering.
--
newtype VoiceMsg = VoiceMsg { getVoiceMsg :: Word8 -> VoiceEvent }

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


vinstrument :: Word8 -> VoiceMsg
vinstrument inst = VoiceMsg $ \ch -> ProgramChange ch inst