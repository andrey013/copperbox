
module MidiDatatypes (
    -- * MidiFile representation
    MidiFile(..),
    -- * File header with basic information
    Header(..),
    -- * Track - a series of events
    Track(..),
    -- * Header format
    HFormat(..),
    -- * Representation of delta times as a division of quarter notes
    TimeDivision(..),
    -- * Type of a text meta-event
    TextType(..),
    -- * A midi event paired with the delta time of its onset
    Event(..), 
    -- * A control or meta event
    EventType(..),
    -- * Scale type - used for setting key signature
    Scale(..)
  ) where

import Data.Int
import Data.Word
import Data.ByteString (ByteString)


data MidiFile = MidiFile Header [Track]
  deriving (Eq,Show,Read)

data Header = Header HFormat Word16 TimeDivision
  deriving (Eq,Show,Read)
              
newtype Track = Track [Event]
  deriving (Eq,Show,Read)
           
data HFormat 
  = MF0     -- ^ single multi-channel track 
  | MF1     -- ^ 1 or more simultaneous tracks
  | MF2     -- ^ 1 or more sequential tracks
  deriving (Eq, Enum, Show, Read) 


data TimeDivision 
  = FPS Word16    -- ^ frames per second
  | TPB Word16    -- ^ ticks per beat
  deriving (Eq,Show,Read)
                                             

data TextType 
  = GENERIC_TEXT 
  | COPYWRIGHT_NOTICE 
  | SEQUENCE_NAME 
  | INSTRUMENT_NAME
  | LYRICS 
  | MARKER 
  | CUE_POINT 
  deriving (Eq,Show,Enum,Read) 
  
data Event = Event DeltaTime EventType 
  deriving (Eq,Show,Read)

type DeltaTime = Word32


data EventType 
  = NoteOff             Word8 Word8 Word8   -- ^ chan x note x velocity
  | NoteOn              Word8 Word8 Word8   -- ^ chan x note x velocity
  | NoteAftertouch      Word8 Word8 Word8   -- ^ chan x note x value
  | Controller          Word8 Word8 Word8   -- ^ chan x type x value
  | ProgramChange       Word8 Word8         -- ^ chan x num  
  | ChanAftertouch      Word8 Word8         -- ^ chan x value
  | PitchBend           Word8 Word16        -- ^ chan x value 
  | TextEvent           TextType String     -- ^ text_type x contents
  | SequenceNumber      Word16              -- ^ sequence_number
  | ChannelPrefix       Word8               -- ^ channel
  | EndOfTrack                              -- ^ no contents
  | SetTempo            Word32              -- ^ microseconds per quarter-note
  | SMPTEOffset         Word8 Word8 Word8 Word8 Word8   -- ^ hour x minute x second x frac x subfrac
  | TimeSignature       Word8 Word8 Word8 Word8         -- ^ numerator x denominator x metronome x number of 32nd notes
  | KeySignature        Int8 Scale          -- ^ key_type x scale_type
  | SSME                Word32 ByteString   -- ^ sequencer specific meta-event - length x data
  | Sysex               Word32 ByteString   -- ^ system exclusive event - length x data               
  deriving (Eq,Show,Read)


data Scale = MAJOR | MINOR
  deriving (Eq,Enum,Show,Read)



