
module MidiDatatypes where

import Data.Int
import Data.Word


data MidiFile = MidiFile Header [Track]
  deriving (Eq,Show,Read)

data Header = Header HFormat Word16 TimeDivision
  deriving (Eq,Show,Read)
              
newtype Track = Track [Event]
  deriving (Eq,Show,Read)
           
data HFormat 
  = MF0 -- single multi-channel track 
  | MF1 -- 1+ simultaneous tracks
  | MF2 -- 1+ sequential tracks
  deriving (Eq, Enum, Show, Read) 


data TimeDivision 
  = FPS Word16
  | TPB Word16
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
  = NoteOff             Word8 Word8 Word8   -- chan x note x velocity
  | NoteOn              Word8 Word8 Word8   -- chan x note x velocity
  | NoteAftertouch      Word8 Word8 Word8   -- chan x note x value
  | Controller          Word8 Word8 Word8   -- chan x type x value
  | ProgramChange       Word8 Word8         -- chan x num  
  | ChanAftertouch      Word8 Word8         -- chan x value
  | PitchBend           Word8 Word16        -- chan x value 
  | TextEvent           TextType String     -- text_type x contents
  | SequenceNumber      Word16              -- sequence_number
  | ChannelPrefix       Word8               -- channel
  | EndOfTrack                              -- no contents
  | SetTempo            Word32              -- mspqn
  | SMPTEOffset         Word8 Word8 Word8 Word8 Word8   -- hour x minute x second x frac x subfrac
  | TimeSignature       Word8 Word8 Word8 Word8         -- numer x denom x metro x nps32
  | KeySignature        Int8 Scale          -- key_type x scale_type
  deriving (Eq,Show,Read)


data Scale = MAJOR | MINOR
  deriving (Eq,Enum,Show,Read)



