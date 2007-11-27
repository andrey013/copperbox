
module Sound.Bala.Format.Midi.Datatypes (
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
    Scale(..)
  ) where

import Data.Int
import Data.Word
import Data.ByteString (ByteString)


data MidiFile = MidiFile Header [Track]
  deriving (Eq,Show)

data Header = Header HFormat Word16 TimeDivision
  deriving (Eq,Show)
              
newtype Track = Track [Message]
  deriving (Eq,Show)
           
data HFormat 
  = MF0     -- ^ single multi-channel track 
  | MF1     -- ^ 1 or more simultaneous tracks
  | MF2     -- ^ 1 or more sequential tracks
  deriving (Eq, Enum, Show) 


data TimeDivision 
  = FPS Word16    -- ^ frames per second
  | TPB Word16    -- ^ ticks per beat
  deriving (Eq,Show)
                                             

data TextType 
  = GENERIC_TEXT 
  | COPYWRIGHT_NOTICE 
  | SEQUENCE_NAME 
  | INSTRUMENT_NAME
  | LYRICS 
  | MARKER 
  | CUE_POINT 
  deriving (Eq,Enum,Show) 
  
type Message = (DeltaTime, Event)


type DeltaTime = Word32


data Event 
  = VoiceEvent        VoiceEvent
  | SystemEvent       SystemEvent
  | MetaEvent         MetaEvent
  deriving (Eq,Show)

-- type StatusByte = (Word8,Word8) -- ^ event-type x channel-number

data VoiceEvent 
  = NoteOff             Word8 Word8 Word8   -- ^ chan x note x velocity
  | NoteOn              Word8 Word8 Word8   -- ^ chan x note x velocity
  | NoteAftertouch      Word8 Word8 Word8   -- ^ chan x note x value
  | Controller          Word8 Word8 Word8   -- ^ chan x type x value
  | ProgramChange       Word8 Word8         -- ^ chan x num  
  | ChanAftertouch      Word8 Word8         -- ^ chan x value
  | PitchBend           Word8 Word16        -- ^ chan x value
  deriving (Eq,Show)
  
  
data SystemEvent 
  = SysEx               Word32 ByteString   -- ^ system exclusive event - length x data               
  | DataEvent           Word8               -- 0..127
  deriving (Eq,Show)

data MetaEvent
  = TextEvent           TextType String     -- ^ text_type x contents
  | SequenceNumber      Word16              -- ^ sequence_number
  | ChannelPrefix       Word8               -- ^ channel
  | EndOfTrack                              -- ^ no contents
  | SetTempo            Word32              -- ^ microseconds per quarter-note
  | SMPTEOffset         Word8 Word8 Word8 Word8 Word8   -- ^ hour x minute x second x frac x subfrac
  | TimeSignature       Word8 Word8 Word8 Word8         -- ^ numerator x denominator x metronome x number of 32nd notes
  | KeySignature        Int8 Scale          -- ^ key_type x scale_type
  | SSME                Word32 ByteString   -- ^ sequencer specific meta-event - length x data
  deriving (Eq,Show)
  
data Scale = MAJOR | MINOR
  deriving (Eq,Enum,Show,Read)



