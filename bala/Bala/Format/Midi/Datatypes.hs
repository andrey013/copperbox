
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Midi.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Syntax tree for MIDI files.
--
--------------------------------------------------------------------------------


module Bala.Format.Midi.Datatypes (
    -- * MidiFile representation
    MidiFile(..),
    -- * File header with basic information
    Header(..),
    -- * Track - a series of events
    Track(..),
    -- * Header format
    HFormat(..),
    
    DeltaTime,

    -- * A midi event paired with the delta time of its onset
    Message, 
    -- * A control or meta event
    Event(..),
    
    VoiceEvent(..),
    SystemEvent(..),
    MetaEvent(..),
    
    -- * Representation of delta times as a division of quarter notes
    TimeDivision(..),
    -- * Type of a text meta-event
    TextType(..),
    
    -- * Scale type - used for setting key signature
    ScaleType(..),
    varlenSplit
  ) where

import Data.Bits
import Data.Int
import Data.Word



data MidiFile = MidiFile Header [Track]
  deriving (Eq,Show)

-- | @'Header' fmt nt td@ 
--
-- @fmt@ - file format - see @'HFormat'@, 
-- @nt@  - number of tracks,
-- @td@  - @'TimeDivision'@, often 480 ticks per beat.
-- The header is the start of a MIDI file, it is indicated by the 
-- marker \'@MThd@\'.   
data Header = Header HFormat Word16 TimeDivision
  deriving (Eq,Show)

-- | @'Track' xs@ 
--
-- @xs@ - list of @'Message'@. In MIDI files, the start of a track is
-- indicated by the marker \'@MTrk@\'.  
newtype Track = Track [Message]
  deriving (Eq,Show)

-- | @'HFormat'@ 
--
-- The file format - in a MIDI file this is a big-endian word16 with 0,1 or 2
-- being the only valid values. 
data HFormat 
    -- | Format 0 file - single multi-channel track.
    = MF0 
    -- | Format 1 file - 1 or more tracks, played simultaneously.
    | MF1
    -- | Format 2 file - 1 or more independent tracks.
    | MF2
  deriving (Eq, Enum, Show) 

-- | @'TimeDivision'@ 
--
-- Defines the default unit of time in the MIDI file.
data TimeDivision 
    -- | frames per second.
    = FPS Word16
    -- | ticks per beat, i.e. the number of units for a quater note.
    | TPB Word16    
  deriving (Eq,Show)
                                             
-- | @'TextType'@ 
--
-- Signal the type of a text meta event.
data TextType 
    = GENERIC_TEXT 
    | COPYRIGHT_NOTICE 
    | SEQUENCE_NAME 
    | INSTRUMENT_NAME
    | LYRICS 
    | MARKER 
    | CUE_POINT 
  deriving (Eq,Enum,Ord,Show) 

-- | @'Message'@ 
--
-- MIDI messages are pairs of @'DeltaTime'@ and @'Event'@. 
-- Sequential messages with delta time 0 will be played simultaneously.  
type Message = (DeltaTime, Event)

-- | @'DeltaTime'@ 
--
-- All time values in a MIDI track are represented as a \delta\ from the 
-- previous event rather than an absolute time. 
-- Although DeltaTime is a type synonym for Word32, in MIDI files it is 
-- represented as a @varlen@ to save space. 
type DeltaTime = Word32

-- | @'Event'@ 
--
-- MIDI has three types of event.
data Event 
    -- | Meta events - usually interpretable (e.g. @end-of-track@, @set-tempo@).
    = MetaEvent         MetaEvent
    -- | Voice events (e.g @note-on@, @note-off@) are relayed to specific
    -- channels.
    | VoiceEvent        VoiceEvent
    -- | SysEx - system exclusive - events. Usually synthesizer specific.
    | SystemEvent       SystemEvent
  deriving (Eq,Show,Ord)

-- | @'VoiceEvent'@ 
--
-- Voice events control the output of the synthesizer.
data VoiceEvent 
    -- | @NoteOff chan note velocity@ - turn off a sounding note.
    = NoteOff             Word8 Word8 Word8   
    -- | @NoteOn chan note velocity@ - start playing a note.
    | NoteOn              Word8 Word8 Word8
    -- | @NoteAftertouch chan note value@ - change in pressure applied to 
    -- the synthesizer key. 
    | NoteAftertouch      Word8 Word8 Word8   
    -- | @Controller chan type value@ - controller change to the channel,
    -- e.g. by a footswitch.
    | Controller          Word8 Word8 Word8
    -- | @ProgramChange chan num@ - change the instrument playing on the 
    -- specified channel. See 'GMInst' for the instruments available with
    -- General MIDI.
    | ProgramChange       Word8 Word8  
    -- | @ChanAftertouch chan value@ - 
    | ChanAftertouch      Word8 Word8
    -- | @PitchBend chan value@ - change the pitch of a sounding note. 
    -- Often used to approxiamate microtonal tunings.
    | PitchBend           Word8 Word16
  deriving (Eq,Show,Ord)
  
-- | @'SystemEvent'@ 
--
-- \SysEx\ events - these are generally uninterpreted. 
data SystemEvent 
    -- | @SysEx length data@ - an uninterpreted sysex event.          
    = SysEx               Word32 [Word8]
    -- | @DataEvent value@ - value should be in the range 0..127.
    | DataEvent           Word8 
  deriving (Eq,Show,Ord)

-- | @'MetaEvent'@ 
--
-- Meta events - in Format 1 files \general\ events (e.g. text events) should
-- only appear in track 1. \Specific\ events (e.g. end-of-track) should appear
-- in any track where necessary. 
data MetaEvent
    -- | @TextEvent text_type contents@ - a free text field (e.g. copyright).
    -- The contents can notionally be any length.
    = TextEvent           TextType String
    -- | @SequenceNumber value@ - Format 1 files - only track 1 should have a 
    -- sequence number. Format 2 files - a sequence number should identify 
    -- each track. 
    -- This should occur at the start of a track, before any non-zero time 
    -- events.
    | SequenceNumber      Word16
    -- | @ChannelPrefix chan@ - relay all meta and sysex events to the 
    -- given channel.
    | ChannelPrefix       Word8
    -- | @EndOfTrack@ - indicated end of track. 
    | EndOfTrack
    -- | @SetTempo mspqn@ - microseconds per quarter-note.
    | SetTempo            Word32
    -- | @SMPTEOffset hour  minute second frac subfrac@ - the SMPTE time when 
    -- a track should start. This should occur at the start of a track,
    -- before any non-zero time events.
    | SMPTEOffset         Word8 Word8 Word8 Word8 Word8
    
    -- | @TimeSignature numerator denominator metro num-32nd-notes@ - time 
    -- signature.
    | TimeSignature       Word8 Word8 Word8 Word8
    
    -- | @KeySignature key_type scale_type@ - key_type is the number of 
    -- sharps (postive numbers) or flats (negative numbers), 
    -- e.g. (-1) is 1 flat.
    -- 'ScaleType' indicates major or minor.  
    | KeySignature        Int8 ScaleType
    
    -- | @SSME length data@ - sequencer specific meta-event.
    | SSME                Word32 [Word8]
  deriving (Eq,Show,Ord)

-- | @'ScaleType'@ 
--
-- Scale type - @major@ or @minor@.  
data ScaleType = MAJOR | MINOR
  deriving (Eq,Enum,Ord,Show)

--------------------------------------------------------------------------------
-- helper for varlen
--------------------------------------------------------------------------------
-- | @varlenSplit@ - reduce a varlen into a list of Word8. 
varlenSplit :: Word32 -> [Word8]
varlenSplit i | i < 0x80        = [fromIntegral i]
              | i < 0x4000      = [wise i 7, wise i 0]
              | i < 0x200000    = [wise i 14, wise i 7, wise i 0] 
              | otherwise       = [wise i 21, wise i 14, wise i 7, wise i 0] 
  where         
    wise i 0 = fromIntegral $ i .&. 0x7F
    wise i n = fromIntegral $ i `shiftR` n   .&.  0x7F  .|.  0x80;
    

