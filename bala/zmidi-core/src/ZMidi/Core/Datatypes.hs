{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (at least generalized newtype deriving)
--
-- Concrete syntax tree for MIDI files.
--
-- Values are sometimes not interpreted. This means that the
-- the data types do not fully represent the sematics of the 
-- data, but all the data is either stored in the tree or 
-- synthesizeable.
-- 
-- @ readFile >>= writeFile @ will produce an identical binary \[1\]. 
--
-- \[1\] Or it should, failure indicates a bug...
--
--------------------------------------------------------------------------------


module ZMidi.Core.Datatypes 
  (
  -- * MidiFile syntax.
    DeltaTime
  , TagByte

  , MidiFile(..)
  , MidiHeader(..)  
  , MidiTrack(..)
  , MidiFormat(..)
  , MidiMessage
  , MidiEvent(..)

  , MidiDataEvent(..)
  , MidiVoiceEvent(..)
  , MidiSysExEvent(..)
  , MidiSysCommonEvent(..)
  , MidiSysRealTimeEvent(..)
  , MidiMetaEvent(..)
  , MidiTimeDivision(..)
  , MidiTextType(..)
  , MidiScaleType(..)
    
  ) where

import Data.Int
import Data.Word



-- | All time values in a MIDI track are represented as a \delta\ 
-- from the previous event rather than an absolute time. 
--
-- DeltaTime is a newtype wrapper over Word32, note that in MIDI 
-- files it is represented as a @varlen@ to save space rather than 
-- a four byte number. 
--
newtype DeltaTime = DeltaTime { getDeltaTime :: Word32 }
  deriving (Enum,Eq,Ord,Num,Integral,Real)

instance Show DeltaTime where
  showsPrec p = showsPrec p . getDeltaTime

-- | TagByte is an alias to 'Word8'.
--
type TagByte = Word8


-- | 'MidiFile' : @ header * tracks @
--
data MidiFile = MidiFile 
      { mf_header         :: MidiHeader
      , mf_tracks         :: [MidiTrack]
      }
  deriving (Eq,Show)

-- | 'Header' : @ format * num_tracks * time_division @ 
--
-- 'TimeDivision' is often 384 or 480 ticks per beat.
--
-- The header is the start of a MIDI file, it is indicated by the 
-- 4 character marker @MThd@.   
--
data MidiHeader = MidiHeader 
      { hdr_format        :: MidiFormat
      , num_tracks        :: Word16
      , time_division     :: MidiTimeDivision
      }
  deriving (Eq,Show)

-- | 'Track' : @ [message] @
--
-- In MIDI files, the start of a track is indicated by the 4 
-- character marker @MTrk@.  
--
newtype MidiTrack = MidiTrack { getTrackMessages :: [MidiMessage] }
  deriving (Eq,Show)

-- | The file format - in a MIDI file this is a big-endian 
-- word16 with 0,1 or 2 being the only valid values. 
--
data MidiFormat 
    -- | Format 0 file - single multi-channel track.
    = MF0 
    -- | Format 1 file - 1 or more tracks, played simultaneously.
    | MF1
    -- | Format 2 file - 1 or more independent tracks.
    | MF2
  deriving (Eq, Enum, Show) 

-- | Default unit of time in the MIDI file.
--
data MidiTimeDivision 
    -- | Frames-per-second.
    --
    = FPS Word16
    
    -- | Ticks-per-beat, i.e. the number of units for a quarter 
    -- note.
    --
    | TPB Word16    
  deriving (Eq,Show)
                                             
-- | Enumeration of the text meta event types.
--
data MidiTextType 
    = GENERIC_TEXT 
    | COPYRIGHT_NOTICE 
    | SEQUENCE_NAME 
    | INSTRUMENT_NAME
    | LYRICS 
    | MARKER 
    | CUE_POINT 
  deriving (Eq,Enum,Ord,Show) 


-- | MIDI messages are pairs of 'DeltaTime' and 'Event' wrapped in 
-- a newtype. 
--
-- Sequential messages with delta time 0 are played 
-- simultaneously.  
--
type MidiMessage = (DeltaTime, MidiEvent)


-- Note, the Ord instance for pairs is very useful for rendering.
-- When we have have a NoteOn and a NoteOff on the same channel at the 
-- same time we want the NoteOff played first. An ordinary sort will 
-- give us this.

     


-- | Recognised event types - some types ('DataEvent' and 
-- 'SysEx') are not interpreted.
--
data MidiEvent 
    -- | Data event - just initial tag byte, 
    -- uninterpreted
    --
    = DataEvent         MidiDataEvent

    -- | Voice event (e.g @note-on@, @note-off@) are relayed to specific
    -- channels.
    --
    | VoiceEvent        MidiVoiceEvent


    -- | SysEx - system exclusive event. Usually synthesizer 
    -- specific, not interpreted.
    --
    | SysExEvent        MidiSysExEvent


    -- | SysCommon - system common event.
    --
    | SysCommonEvent    MidiSysCommonEvent


    -- | SysRealTime - system realtime event.
    --
    | SysRealTimeEvent  MidiSysRealTimeEvent


    -- | Meta event - interpreted (e.g. @end-of-track@, 
    -- @set-tempo@).
    --
    | MetaEvent         MidiMetaEvent


  deriving (Eq,Show,Ord)

-- | Data events are events with tags from 0x00 to 0x7F. 
-- 
-- Data events have no payload - they are represented only by the
-- tag byte.  
--
newtype MidiDataEvent = MidiDataEvent { getTagByte :: TagByte }
  deriving (Eq,Ord,Show)

-- | Voice events control the output of the synthesizer.
--
-- Note - the constructors are not in the the same order as their 
-- byte values. Controller and ProgramChange are higher than they 
-- /naturally/ occur so the will come first after a comparison or 
-- sort.
--
-- When generating MIDI, Controller and ProgramChange events 
-- should be signalled before NoteOn or NoteOff events at the same 
-- delta-time. Changing the order of the constructors helps to 
-- sort for this.
--
data MidiVoiceEvent 
    -- | @ channel * controller_number * value @ 
    -- 
    -- Controller change, e.g. by a footswitch.
    --
    = Controller          Word8 Word8 Word8

    -- | @ channel * program_number @ - change the instrument 
    -- playing on the specified channel. For playback on 
    -- computers (rather than synthesizers) the program numbers
    -- will correspond to the /General MIDI/ instrument numbers.
    --
    | ProgramChange       Word8 Word8

    -- | @ channel * note * velocity @ 
    -- 
    -- Turn off a sounding note.
    --
    | NoteOff             Word8 Word8 Word8   

    -- | @ channel * note * velocity @ 
    -- 
    -- Start playing a note.
    --
    | NoteOn              Word8 Word8 Word8

    -- | @ channel * note * pressure_value @ 
    -- 
    -- Change of pressure applied to the synthesizer key. 
    --
    | NoteAftertouch      Word8 Word8 Word8     

    -- | @ channel * pressure_value @ 
    -- 
    | ChanAftertouch      Word8 Word8

    -- | @ channel * value @ 
    -- 
    -- Change the pitch of a sounding note. Often used to 
    -- approximate microtonal tunings.
    -- 
    -- NOTE - currently value is uninterpreted.
    --
    | PitchBend           Word8 Word16
  deriving (Eq,Show,Ord)

-- | \SysEx\ - system exclusive event. 
--
data MidiSysExEvent
    -- | @ length * data @ 
    -- 
    -- An uninterpreted sys-ex event.
    --
    = SysEx               Word32 [Word8]
  deriving (Eq,Show,Ord)


-- | System common event.
--
-- Common information for all channels in a system. 
--
-- These events may not be pertinent to MIDI files generated on a 
-- computer (as opposed to MIDI generated by a synthesizer or 
-- sequencer).
--
data MidiSysCommonEvent
    -- | Time code quarter frame.
    -- 
    -- Note the payload is really a byte split into two 4-bit 
    -- values, however here it is uninterpreted.
    --
    = QuarterFrame      Word8
    
    -- | Song position pointer.
    --
    | SongPosPointer    Word8       Word8

    -- | @ song_number @
    --
    -- Song number should be in the range 0..127.
    --
    | SongSelect        Word8

    -- | Tag should be limited to 0xF4 or 0xF5.
    --
    -- Other values would indicate either a badly formed MIDI
    -- file or a failure with the parser.
    --
    | Common_undefined  TagByte

    -- | Tune request message for analogue synthesizers.
    --
    | TuneRequest
    
    -- | End-of-system-exclusive message.
    | EOX
  deriving (Eq,Show,Ord)


-- | System real-time event.
--
-- These events may not be pertinent to MIDI files generated on a 
-- computer (as opposed to MIDI generated by a synthesizer or 
-- sequencer).
--
data MidiSysRealTimeEvent
    -- | Timing signal.
    --      
    = TimingClock
    
    -- | Tag should be limited to either 0xF9 or 0xFD.
    --
    -- Other values would indicate either a badly formed MIDI
    -- file or a failure with the parser.
    --
    | RT_undefined      TagByte
    
    -- | Start playing a sequence.
    -- 
    | StartSequence
    
    -- | Continue playing a stopped sequence.
    --
    | ContinueSequence

    -- | Stop playing a sequence.
    --
    | StopSequence

    -- | Synchronization pulse...
    -- 
    | ActiveSensing

    -- | Reset to power-up status.
    --
    | SystemReset
  deriving (Eq,Show,Ord)






-- | Meta event 
-- 
-- In Format 1 files general events (e.g. text events) should
-- only appear in track 1. Certain events (e.g. end-of-track) 
-- can appear in any track where necessary. 
--
data MidiMetaEvent

    -- | @ text_type * contents @ 
    -- 
    -- Free text field (e.g. copyright statement). The contents 
    -- can notionally be any length.
    --
    = TextEvent           MidiTextType String

    -- | @ value @ 
    -- 
    -- Format 1 files - only track 1 should have a sequence 
    -- number. 
    --
    -- Format 2 files - a sequence number should identify each 
    -- track.
    --  
    -- The sequence number event should occur at the start of a 
    -- track, before any non-zero time events.
    --
    | SequenceNumber      Word16

    -- | @ 1 * channel @ 
    -- 
    -- Relay all meta and sys-ex events to the given channel.
    --
    -- The first byte should always be 1.
    -- 
    | ChannelPrefix       Word8 Word8

    -- | End-of-track event. 
    --
    | EndOfTrack

    -- | @ microseconds_per_quarter_note @
    --
    | SetTempo            Word32

    -- | @ hour * minute * second * frac * subfrac @ 
    -- 
    -- The SMPTE time when a track should start. This event 
    -- should occur at the start of a track, before any non-zero 
    -- time events.
    --
    | SMPTEOffset         Word8 Word8 Word8 Word8 Word8
    
    -- | @ numerator * denominator * metro * num_32nd_notes @ 
    --
    | TimeSignature       Word8 Word8 Word8 Word8
    
    -- | @ key_type * scale_type @ 
    --
    -- @key_type@ is the number of sharps (postive numbers) or 
    -- flats (negative numbers), e.g. (-1) is 1 flat.
    --
    -- @scale_type@ indicates major or minor.  
    --
    | KeySignature        Int8 MidiScaleType
    
    -- | @ length * data@ 
    -- 
    -- Sequencer specific meta-event - uninterpreted.
    --
    | SSME                Word32 [Word8]

  deriving (Eq,Show,Ord)

-- | Scale type - @major@ or @minor@.  
--
data MidiScaleType = MAJOR | MINOR
  deriving (Eq,Enum,Ord,Show)


