--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Midi.SyntaxElements
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A nicer syntax layer built on the abstract syntax provided by the datatypes.
--
--------------------------------------------------------------------------------


module Bala.Format.Midi.SyntaxElements (
  header, format0_header, format1_header, format2_header,
  fps, tpb,
  
  messageZero,
  
  noteOff,          noteOff_message,
  noteOn,           noteOn_message,
  noteAftertouch,   noteAftertouch_message,
  controller,       controller_message,
  programChange,    programChange_message,
  chanAftertouch,   chanAftertouch_message,
  pitchBend,        pitchBend_message,
  
  textEvent,
  genericText,      genericText_zero,
  copyrightNotice,  copyrightNotice_zero,
  sequenceName,     sequenceName_zero,
  instrumentName,   instrumentName_zero,
  lyrics,           lyrics_zero,
  marker,           marker_zero,
  cue_point,        cue_point_zero,
  
  sequenceNumber,   sequenceNumber_message,   sequenceNumber_zero,
  channelPrefix,    channelPrefix_message,    channelPrefix_zero,
  endOfTrack,       endOfTrack_message,       endOfTrack_zero,
  setTempo,         setTempo_message,         setTempo_zero,
  smpteOffset,      smpteOffset_message,      smpteOffset_zero,
  timeSignature,    timeSignature_message,    timeSignature_zero,
  keySignature,     keySignature_message,     keySignature_zero,
  
  majorScale, minorScale,

  -- Major scales with sharps
  c_major, g_major, d_major, a_major, 
  e_major, b_major, f_sharp_major, c_sharp_major, 
  
  -- Major scales with flats
  f_major, b_flat_major, e_flat_major, a_flat_major, 
  d_flat_major, g_flat_major, c_flat_major, 
  
  
  -- Minor scales with sharps
  a_minor, e_minor, b_minor, f_sharp_minor,
  c_sharp_minor, g_sharp_minor, d_sharp_minor, a_sharp_minor, 
  
  -- Minor scales with flats
  d_minor, g_minor, c_minor, f_minor, 
  b_flat_minor, e_flat_minor, a_flat_minor
  
  ) where

import Bala.Base.BaseExtra
import Bala.Format.Midi.Datatypes

import Data.Word


header :: Integral a => HFormat -> a -> TimeDivision -> Header
header fmt i td = Header fmt (fromIntegral i) td

format0_header :: TimeDivision -> Header
format0_header = header MF0 1

format1_header :: Integral a => a -> TimeDivision -> Header
format1_header = header MF1

format2_header :: Integral a => a -> TimeDivision -> Header
format2_header = header MF2



fps :: Integral a => a -> TimeDivision
fps = FPS . fromIntegral 

tpb :: Integral a => a -> TimeDivision
tpb = TPB . fromIntegral 

    
    
    
message :: Integral dt => dt -> Event -> Message
message dt e = (fromIntegral dt, e)

messageZero :: Event -> Message
messageZero e = (0, e)


noteOff                     :: Integral a => a -> a -> a -> Event
noteOff ch note vel         = VoiceEvent $ 
    NoteOff (fromIntegral ch) (fromIntegral note) (fromIntegral vel)
    
noteOff_message             :: (Integral dt, Integral a) 
                            => dt -> a -> a -> a -> Message
noteOff_message dt          = message dt `triap` noteOff


noteOn                      :: Integral a => a -> a -> a -> Event
noteOn ch note vel          = VoiceEvent $ 
    NoteOn (fromIntegral ch) (fromIntegral note) (fromIntegral vel)
    
noteOn_message              :: (Integral dt, Integral a) 
                            => dt -> a -> a -> a -> Message
noteOn_message dt           = message dt `triap` noteOn


noteAftertouch              :: Integral a => a -> a -> a -> Event
noteAftertouch ch note val  = VoiceEvent $ 
    NoteAftertouch (fromIntegral ch) (fromIntegral note) (fromIntegral val)
    
noteAftertouch_message      :: (Integral dt, Integral a) 
                            => dt -> a -> a -> a -> Message
noteAftertouch_message dt   = message dt `triap` noteAftertouch

controller                  :: Integral a => a -> a -> a -> Event
controller ch note val      = VoiceEvent $ 
    Controller (fromIntegral ch) (fromIntegral note) (fromIntegral val)
    
controller_message          :: (Integral dt, Integral a) 
                            => dt -> a -> a -> a -> Message
controller_message dt       = message dt `triap` controller



programChange               :: Integral a => a -> a -> Event
programChange ch num        = VoiceEvent $ 
    ProgramChange (fromIntegral ch) (fromIntegral num)
    
programChange_message       :: (Integral dt, Integral a) 
                            => dt -> a -> a -> Message
programChange_message dt    = message dt `dyap` programChange

chanAftertouch              :: Integral a => a -> a -> Event
chanAftertouch ch val       = VoiceEvent $ 
    ChanAftertouch (fromIntegral ch) (fromIntegral val)
    
chanAftertouch_message      :: (Integral dt, Integral a) 
                            => dt -> a -> a -> Message
chanAftertouch_message dt   = message dt `dyap` chanAftertouch


pitchBend                   :: Integral a => a -> a -> Event
pitchBend ch val            = VoiceEvent $ 
    PitchBend (fromIntegral ch) (fromIntegral val)
    
pitchBend_message           :: (Integral dt, Integral a) 
                            => dt -> a -> a -> Message
pitchBend_message dt        = message dt `dyap` pitchBend


{-   

    | PitchBend           Word8 Word16
    
-}    


textEvent                   :: TextType -> String -> Event
textEvent                   = MetaEvent `dyap` TextEvent

genericText                 :: String -> Event
genericText                 = textEvent GENERIC_TEXT 

genericText_zero            :: String -> Message
genericText_zero            = message 0 . genericText


copyrightNotice             :: String -> Event
copyrightNotice             = textEvent COPYRIGHT_NOTICE 

copyrightNotice_zero        :: String -> Message
copyrightNotice_zero        = message 0 . copyrightNotice

sequenceName                :: String -> Event
sequenceName                = textEvent SEQUENCE_NAME 

sequenceName_zero           :: String -> Message
sequenceName_zero           = message 0 . sequenceName


instrumentName              :: String -> Event
instrumentName              = textEvent INSTRUMENT_NAME 

instrumentName_zero         :: String -> Message
instrumentName_zero         = message 0 . instrumentName


lyrics                      :: String -> Event
lyrics                      = textEvent LYRICS

lyrics_zero                 :: String -> Message
lyrics_zero                 = message 0 . lyrics

marker                      :: String -> Event
marker                      = textEvent MARKER

marker_zero                 :: String -> Message
marker_zero                 = message 0 . marker


cue_point                   :: String -> Event
cue_point                   = textEvent CUE_POINT

cue_point_zero              :: String -> Message
cue_point_zero              = message 0 . cue_point
  
    
sequenceNumber              :: Integral a => a -> Event
sequenceNumber              = MetaEvent . SequenceNumber . fromIntegral

sequenceNumber_message      :: (Integral dt, Integral a) => dt -> a -> Message
sequenceNumber_message dt   = message dt . sequenceNumber

sequenceNumber_zero         :: Integral a => a -> Message
sequenceNumber_zero         = sequenceNumber_message 0


channelPrefix               :: Integral a => a -> Event
channelPrefix               = MetaEvent . ChannelPrefix . fromIntegral

channelPrefix_message       :: (Integral dt, Integral a) => dt -> a -> Message
channelPrefix_message dt    = message dt . channelPrefix

channelPrefix_zero          :: Integral a => a -> Message
channelPrefix_zero          = channelPrefix_message 0


endOfTrack                  :: Event
endOfTrack                  = MetaEvent $ EndOfTrack

endOfTrack_message          :: Integral a => a -> Message
endOfTrack_message dt       = message dt endOfTrack

endOfTrack_zero             :: Message
endOfTrack_zero             = endOfTrack_message 0


setTempo                    :: Integral a => a -> Event
setTempo                    = MetaEvent . SetTempo . fromIntegral

setTempo_message            :: (Integral dt, Integral a) => dt -> a -> Message
setTempo_message dt         = message dt . setTempo

setTempo_zero               :: Integral a => a -> Message
setTempo_zero               = setTempo_message 0



smpteOffset           :: Integral a => a -> a -> a -> a -> a -> Event
smpteOffset hr mn sc fr sf  = 
    MetaEvent $ SMPTEOffset (fromIntegral hr) 
                            (fromIntegral mn) 
                            (fromIntegral sc) 
                            (fromIntegral fr) 
                            (fromIntegral sf)

smpteOffset_message   :: (Integral dt, Integral a) 
                      => dt -> a -> a -> a -> a -> a -> Message
smpteOffset_message dt hr mn sc fr sf = message dt $ smpteOffset hr mn sc fr sf

smpteOffset_zero      :: Integral a => a -> a -> a -> a -> a -> Message
smpteOffset_zero      = smpteOffset_message 0


timeSignature           :: Integral a => a -> a -> a -> a -> Event
timeSignature nr dr met n = 
    MetaEvent $ TimeSignature (fromIntegral dr) 
                              (fromIntegral nr) 
                              (fromIntegral met) 
                              (fromIntegral n)

timeSignature_message   :: (Integral dt, Integral a) 
                        => dt -> a -> a -> a -> a -> Message
timeSignature_message dt nr dr met n = message dt $ timeSignature nr dr met n

timeSignature_zero      :: Integral a => a -> a -> a -> a -> Message
timeSignature_zero      = timeSignature_message 0


keySignature                :: Integral a => a -> ScaleType -> Event
keySignature n t            = MetaEvent $ KeySignature (fromIntegral n) t


keySignature_message        :: (Integral dt, Integral a) 
                            => dt -> a -> ScaleType -> Message
keySignature_message dt     = message dt `dyap` keySignature

keySignature_zero           :: Integral a => a -> ScaleType -> Message
keySignature_zero           = keySignature_message 0

-- Nothing for SSME

 
majorScale :: ScaleType
majorScale = MAJOR 

minorScale :: ScaleType
minorScale = MINOR  

-- Major scales with sharps
c_major             :: Event
c_major             = keySignature 0 majorScale

g_major             :: Event
g_major             = keySignature 1 majorScale

d_major             :: Event
d_major             = keySignature 2 majorScale

a_major             :: Event
a_major             = keySignature 3 majorScale

e_major             :: Event
e_major             = keySignature 4 majorScale

b_major             :: Event
b_major             = keySignature 5 majorScale

f_sharp_major       :: Event
f_sharp_major       = keySignature 6 majorScale

c_sharp_major       :: Event
c_sharp_major       = keySignature 7 majorScale

-- Major scales with flats
f_major             :: Event
f_major             = keySignature (-1) majorScale

b_flat_major        :: Event
b_flat_major        = keySignature (-2) majorScale

e_flat_major        :: Event
e_flat_major        = keySignature (-3) majorScale

a_flat_major        :: Event
a_flat_major        = keySignature (-4) majorScale

d_flat_major        :: Event
d_flat_major        = keySignature (-5) majorScale

g_flat_major        :: Event
g_flat_major        = keySignature (-6) majorScale

c_flat_major        :: Event
c_flat_major        = keySignature (-7) majorScale



-- Minor scales with sharps
a_minor             :: Event
a_minor             = keySignature 0 minorScale

e_minor             :: Event
e_minor             = keySignature 1 minorScale

b_minor             :: Event
b_minor             = keySignature 2 minorScale

f_sharp_minor       :: Event 
f_sharp_minor       = keySignature 3 minorScale

c_sharp_minor       :: Event 
c_sharp_minor       = keySignature 4 minorScale

g_sharp_minor       :: Event 
g_sharp_minor       = keySignature 5 minorScale

d_sharp_minor       :: Event 
d_sharp_minor       = keySignature 6 minorScale

a_sharp_minor       :: Event 
a_sharp_minor       = keySignature 7 minorScale

-- Minor scales with flats
d_minor             :: Event
d_minor             = keySignature (-1) minorScale

g_minor             :: Event
g_minor             = keySignature (-2) minorScale

c_minor             :: Event
c_minor             = keySignature (-3) minorScale

f_minor             :: Event
f_minor             = keySignature (-4) minorScale

b_flat_minor        :: Event
b_flat_minor        = keySignature (-5) minorScale

e_flat_minor        :: Event
e_flat_minor        = keySignature (-6) minorScale

a_flat_minor        :: Event
a_flat_minor        = keySignature (-7) minorScale


