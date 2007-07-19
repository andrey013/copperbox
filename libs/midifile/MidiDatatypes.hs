
module MidiDatatypes where

import Data.Int
import Data.Word


data MidiFile = MidiFile Header [Track]
  deriving (Eq,Show,Read)

data Header = Header {track_type :: HFormat,
                      num_tracks :: Word16,
                      time_div   :: TimeDivision }
  deriving (Eq,Show,Read)
              
newtype Track = Track {events :: [Event]}
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
  
data Event = Event { time_stamp :: DeltaTime,
                     event_type :: EventType}  
  deriving (Eq,Show,Read)

type DeltaTime = Word32


data EventType 
  = NoteOff             {channel::Word8,   note::Word8,     velocity::Word8}
  | NoteOn              {channel::Word8,   note::Word8,     velocity::Word8}
  | NoteAftertouch      {channel::Word8,   note::Word8,     value::Word8}
  | Controller          {channel::Word8,   ctype::Word8,    value::Word8}
  | ProgramChange       {channel::Word8,   number::Word8}  
  | ChanAftertouch      {channel::Word8,   value::Word8}
  | PitchBend           {channel::Word8,   cvalue::Word16}
  | TextEvent           {text_type::TextType, text_contents::String}
  | SequenceNumber      {sequence_number::Word16}
  | ChannelPrefix       {channel::Word8} 
  | EndOfTrack          -- no contents
  | SetTempo            {mspqn::Word32}   
  | SMPTEOffset         {hour::Word8,     minute::Word8,        second::Word8, 
                         frac::Word8,    subfrac::Word8}
  | TimeSignature       {numer::Word8,  denom::Word8,     metro::Word8, 
                         nps32::Word8}
  | KeySignature        {key_type::Int8, scale_type::Scale}
  deriving (Eq,Show,Read)


data Scale = MAJOR | MINOR
  deriving (Eq,Enum,Show,Read)



