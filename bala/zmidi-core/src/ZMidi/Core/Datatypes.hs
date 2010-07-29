{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Concrete syntax tree for MIDI files.
--
--------------------------------------------------------------------------------


module ZMidi.Core.Datatypes 
  (
  -- * MidiFile representation
    MidiFile(..)
  
  -- * File header with basic information
  ,  Header(..)
  
  -- * Track - a series of events
  , Track(..)
  
  -- * Header format
  , HFormat(..)
    

  -- * A midi event paired with the delta time of its onset
  , DeltaTime
  , Message
    

  -- * A control or meta event
  , Event(..)
    
  , DataEvent(..)
  , VoiceEvent(..)
  , SysExEvent(..)
  , SysCommonEvent(..)
  , SysRealTimeEvent(..)
  , MetaEvent(..)
    
  -- * Representation of delta times as a division of quarter notes
  , TimeDivision(..)
  -- * Type of a text meta-event
  , TextType(..)
    
  -- * Scale type - used for setting key signature
  , ScaleType(..)


  -- * SplitByte
  , SplitByte(..)
  , splitByte
  , joinByte
    
  -- * Varlen - not explicitly used in the AST
  , Varlen(..)
  , fromVarlen
  , toVarlen

  , hexStr
    
  ) where

import Data.Bits
import Data.Int
import Data.Word
import Numeric (showHex)

type TagByte = Word8

data MidiFile = MidiFile 
      { mf_header         :: Header
      , mf_tracks         :: [Track]
      }
  deriving (Eq,Show)

-- | @'Header' fmt nt td@ 
--
-- @fmt@ - file format - see @'HFormat'@, 
-- @nt@  - number of tracks,
-- @td@  - @'TimeDivision'@, often 384 or 480 ticks per beat.
-- The header is the start of a MIDI file, it is indicated by the 
-- marker \'@MThd@\'.   
--
data Header = Header { 
      hdr_format        :: HFormat,
      num_tracks        :: Word16,
      time_division     :: TimeDivision
    }
  deriving (Eq,Show)

-- | @'Track' xs@ 
--
-- @xs@ - list of @'Message'@. In MIDI files, the start of a track is
-- indicated by the marker \'@MTrk@\'.  
--
newtype Track = Track { getTrack :: [Message] }
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
--
data TimeDivision 
    -- | frames per second.
    = FPS Word16
    -- | ticks per beat, i.e. the number of units for a quater note.
    | TPB Word16    
  deriving (Eq,Show)
                                             
-- | @'TextType'@ 
--
-- Signal the type of a text meta event.
--
data TextType 
    = GENERIC_TEXT 
    | COPYRIGHT_NOTICE 
    | SEQUENCE_NAME 
    | INSTRUMENT_NAME
    | LYRICS 
    | MARKER 
    | CUE_POINT 
  deriving (Eq,Enum,Ord,Show) 

-- | @'DeltaTime'@ 
--
-- All time values in a MIDI track are represented as a \delta\ from the 
-- previous event rather than an absolute time. 
-- Although DeltaTime is a type synonym for Word32, in MIDI files it is 
-- represented as a @varlen@ to save space. 
--
type DeltaTime = Word32

-- | @'Message'@ 
--
-- MIDI messages are pairs of @'DeltaTime'@ and @'Event'@ wrapped in a newtype. 
-- Sequential messages with delta time 0 will be played simultaneously.  
--
type Message = (DeltaTime, Event)


-- Note, the Ord instance for pairs is very useful for rendering.
-- When we have have a NoteOn and a NoteOff on the same channel at the 
-- same time we want the NoteOff played first. An ordinary sort will 
-- give us this.

     


-- | @'Event'@ 
--
-- MIDI has four types of event.
--
data Event 
    -- | Data event - 4 bytes long including initial tag byte, 
    -- uninterpreted
    = DataEvent         DataEvent

    -- | Voice event (e.g @note-on@, @note-off@) are relayed to specific
    -- channels.
    | VoiceEvent        VoiceEvent


    -- | SysEx - system exclusive - events. Usually synthesizer 
    -- specific.
    --
    -- @length * data@ - an uninterpreted sysex event.          
    --
    | SysExEvent        SysExEvent


    -- | SysCommon - system common events.           
    --
    | SysCommonEvent    SysCommonEvent


    -- | SysRealTime - system realtime events.           
    --
    | SysRealTimeEvent  SysRealTimeEvent


    -- | Meta event - usually interpretable (e.g. @end-of-track@, 
    -- @set-tempo@).
    --
    | MetaEvent         MetaEvent


  deriving (Eq,Show,Ord)


data DataEvent = Data4 TagByte Word8 Word8 Word8
  deriving (Eq,Ord,Show)

-- | @'VoiceEvent'@ 
--
-- Voice events control the output of the synthesizer.
-- Note - these are not in the the same order as there byte values
-- Controller and ProgramChange are higher than they /naturally/
-- occur so the will come first after a comparison / sort.
--
data VoiceEvent 
    -- | @Controller chan type value@ - controller change to the channel,
    -- e.g. by a footswitch.
    = Controller          Word8 Word8 Word8

    -- | @ProgramChange chan num@ - change the instrument playing on the 
    -- specified channel. See 'GMInst' for the instruments available with
    -- General MIDI.
    | ProgramChange       Word8 Word8

    -- | @NoteOff chan note velocity@ - turn off a sounding note.
    | NoteOff             Word8 Word8 Word8   

    -- | @NoteOn chan note velocity@ - start playing a note.
    | NoteOn              Word8 Word8 Word8

    -- | @NoteAftertouch chan note value@ - change in pressure applied to 
    -- the synthesizer key. 
    | NoteAftertouch      Word8 Word8 Word8     

    -- | @ChanAftertouch chan value@ - 
    | ChanAftertouch      Word8 Word8

    -- | @PitchBend chan value@ - change the pitch of a sounding note. 
    -- Often used to approxiamate microtonal tunings.
    | PitchBend           Word8 Word16
  deriving (Eq,Show,Ord)

-- | @'SysExEvent'@ 
--
-- \SysEx\ events - these are generally uninterpreted by ZMidi. 
--
data SysExEvent
    -- | @SysEx length data@ - an uninterpreted sysex event.          
    = SysEx               Word32 [Word8]
  deriving (Eq,Show,Ord)


-- | @'SysCommonEvent'@ 
--
-- \SysEx\ events - these are generally uninterpreted by ZMidi. 
--
data SysCommonEvent
    -- | @QuarterFrame@           
    = QuarterFrame      SplitByte
    
    | SongPosPointer    Word8       Word8

    | SongSelect        Word8

    -- | Tag should be limited to 0xF4 or 0xF5.
    --
    | Common_undefined  TagByte

    | TuneRequest
    | EOX
  deriving (Eq,Show,Ord)


-- | @'SysRealTime'@ 
--
-- \SysEx\ events - these are generally uninterpreted by ZMidi. 
--
data SysRealTimeEvent
    -- | @QuarterFrame@           
    = TimingClock
    
    -- | Tag should be limited to either 0xF9 or 0xFD.
    --
    | RT_undefined      TagByte
    | StartSequence
    | ContinueSequence
    | StopSequence
    | ActiveSensing
    | SystemReset
  deriving (Eq,Show,Ord)






-- | @'MetaEvent'@ 
--
-- Meta events - in Format 1 files \general\ events (e.g. text events) should
-- only appear in track 1. \Specific\ events (e.g. end-of-track) should appear
-- in any track where necessary. 
--
data MetaEvent

    -- | @TextEvent text_type contents@ - a free text field (e.g. copyright).
    -- The contents can notionally be any length.
    --
    = TextEvent           TextType String

    -- | @SequenceNumber value@ - Format 1 files - only track 1 should have a 
    -- sequence number. Format 2 files - a sequence number should identify 
    -- each track. 
    -- This should occur at the start of a track, before any non-zero time 
    -- events.
    --
    | SequenceNumber      Word16

    -- | @ChannelPrefix chan@ - relay all meta and sysex events to the 
    -- given channel.
    --
    | ChannelPrefix       Word8 Word8   -- first w8==1

    -- | @EndOfTrack@ - indicated end of track. 
    --
    | EndOfTrack

    -- | @SetTempo mspqn@ - microseconds per quarter-note.
    --
    | SetTempo            Word32

    -- | @SMPTEOffset hour  minute second frac subfrac@ - the SMPTE time when 
    -- a track should start. This should occur at the start of a track,
    -- before any non-zero time events.
    --
    | SMPTEOffset         Word8 Word8 Word8 Word8 Word8
    
    -- | @TimeSignature numerator denominator metro num-32nd-notes@ - time 
    -- signature.
    --
    | TimeSignature       Word8 Word8 Word8 Word8
    
    -- | @KeySignature key_type scale_type@ - key_type is the number of 
    -- sharps (postive numbers) or flats (negative numbers), 
    -- e.g. (-1) is 1 flat.
    -- 'ScaleType' indicates major or minor.  
    --
    | KeySignature        Int8 ScaleType
    
    -- | @SSME length data@ - sequencer specific meta-event.
    --
    | SSME                Word32 [Word8]

  deriving (Eq,Show,Ord)

-- | @'ScaleType'@ 
--
-- Scale type - @major@ or @minor@.  
--
data ScaleType = MAJOR | MINOR
  deriving (Eq,Enum,Ord,Show)



--------------------------------------------------------------------------------

data SplitByte = SB { upper4 :: Word8, lower4 :: Word8 }
  deriving (Eq,Ord,Show)

splitByte :: Word8 -> SplitByte
splitByte i = SB ((i .&. 0xF0) `shiftR` 4) (i .&. 0x0F)

joinByte :: SplitByte -> Word8
joinByte (SB a b) = (a `shiftL` 4) + (b .&. 0x0F)


--------------------------------------------------------------------------------
-- helper for varlen
--------------------------------------------------------------------------------

data Varlen = V1 !Word8
            | V2 !Word8 !Word8
            | V3 !Word8 !Word8 !Word8
            | V4 !Word8 !Word8 !Word8 !Word8
  deriving (Eq,Ord,Show)


up :: Word8 -> Word32
up = fromIntegral . (0x7f .&.)

down :: Word32 -> Word8
down = (0x80 .|.) . fromIntegral

downl :: Word32 -> Word8
downl = (0x7f .&.) . fromIntegral
 

fromVarlen :: Varlen -> Word32
fromVarlen (V1 a)       = up a
fromVarlen (V2 a b)     = (left7 $ up a)  + up b
fromVarlen (V3 a b c)   = (left14 $ up a) + (left7  $ up b) + up c
fromVarlen (V4 a b c d) = (left21 $ up a) + (left14 $ up b) 
                        + (left7  $ up c) + up d

left7     :: Word32 -> Word32
left7     = (`shiftL` 7)

left14    :: Word32 -> Word32
left14    = (`shiftL` 14)

left21    :: Word32 -> Word32
left21    = (`shiftL` 21)

right7    :: Word32 -> Word32
right7    = (`shiftR` 7)

right14   :: Word32 -> Word32
right14   = (`shiftR` 14)

right21   :: Word32 -> Word32
right21   = (`shiftR` 21)

toVarlen :: Word32 -> Varlen
toVarlen i 
    | i < 0x80           = V1 (downl i)
    | i < 0x4000         = V2 (down $ right7 i)  (downl i)
    | i < 0x200000       = V3 (down $ right14 i) (down $ right7  i) (downl i)
    | otherwise          = V4 (down $ right21 i) (down $ right14 i)
                              (down $ right7  i) (downl i)

hexStr :: (Integral a) => a -> String
hexStr i = (showString "0x" . showHex i) "" 
