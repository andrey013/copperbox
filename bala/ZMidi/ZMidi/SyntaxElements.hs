--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.SyntaxElements
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


module ZMidi.SyntaxElements (
    header_fmt0, header_fmt1, header_fmt2,
    fps, tpb,
   
    noteoff,
    noteon, 
    aftertouch,
    controller,
    prgm_change,
    chan_aftertouch,
    pitchbend,
    
    generic_text,
    copyright,
    seq_name,
    inst_name,
    lyrics,
    marker,
    cue_point,
    
    seq_number,
    chan_prefix,
    end_of_track,
    set_tempo,
    smpte_offset,
    time_sign,
    key_sign,     
    
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


import ZMidi.Datatypes

import Data.Word


header :: Integral a => HFormat -> a -> TimeDivision -> Header
header fmt i td = Header fmt (fromIntegral i) td

header_fmt0 :: TimeDivision -> Header
header_fmt0 = header MF0 1

header_fmt1 :: Integral a => a -> TimeDivision -> Header
header_fmt1 = header MF1

header_fmt2 :: Integral a => a -> TimeDivision -> Header
header_fmt2 = header MF2



fps :: Integral a => a -> TimeDivision
fps = FPS . fromIntegral 

tpb :: Integral a => a -> TimeDivision
tpb = TPB . fromIntegral 

    
    
    
msg :: Integral dt => dt -> Event -> Message
msg dt e = Message (fromIntegral dt, e)

msgzero :: Event -> Message
msgzero e = Message (0, e)


coerce2 :: (Integral a1, Integral a2, Num b1, Num b2) 
        => (b1 -> b2 -> o) -> a1 -> a2 -> o
coerce2 f a b = f (fromIntegral a) (fromIntegral b)

coerce3 :: (Integral a1, Integral a2, Integral a3, Num b1, Num b2, Num b3) 
        => (b1 -> b2 -> b3 -> o) -> a1 -> a2 -> a3 -> o
coerce3 f a b c = f (fromIntegral a) (fromIntegral b) (fromIntegral c)    


voiceEventMsg3  :: (Integral dt, Integral a1, Integral a2, Integral a3,
                   Num b1, Num b2, Num b3) 
                => (b1 -> b2 -> b3 -> VoiceEvent)
                -> dt -> a1 -> a2 -> a3 
                -> Message
voiceEventMsg3 f dtime a b c = msg dtime $ VoiceEvent $ (coerce3 f) a b c

voiceEventMsg2  :: (Integral dt, Integral a1, Integral a2, Num b1, Num b2) 
                => (b1 -> b2 -> VoiceEvent)
                -> dt -> a1 -> a2 
                -> Message
voiceEventMsg2 f dtime a b = msg dtime $ VoiceEvent $ (coerce2 f) a b


textEventMsg          :: TextType -> String -> Message
textEventMsg typ txt  = msgzero $ MetaEvent $ TextEvent typ txt


metaEventMsg3   :: (Integral dt, Integral a1, Integral a2, Integral a3,
                   Num b1, Num b2, Num b3) 
                => (b1 -> b2 -> b3 -> MetaEvent)
                -> dt -> a1 -> a2 -> a3 
                -> Message
metaEventMsg3 f dtime a b c = msg dtime $ MetaEvent $ (coerce3 f) a b c

metaEventMsg2   :: (Integral dt, Integral a1, Integral a2, Num b1, Num b2) 
                => (b1 -> b2 -> MetaEvent)
                -> dt -> a1 -> a2 
                -> Message
metaEventMsg2 f dtime a b = msg dtime $ MetaEvent $ (coerce2 f) a b

metaEventMsg    :: (Integral dt, Integral a1, Num b1) 
                => (b1 -> MetaEvent)
                -> dt -> a1 
                -> Message
metaEventMsg f dtime a = msg dtime $ MetaEvent $  f (fromIntegral a)


--------------------------------------------------------------------------------

    
noteoff         :: (Integral dt, Integral a, Integral b, Integral c) 
                => dt -> a -> b -> c -> Message
noteoff         = voiceEventMsg3 NoteOff  

    
noteon          :: (Integral dt, Integral a, Integral b, Integral c) 
                => dt -> a -> b -> c -> Message
noteon          = voiceEventMsg3 NoteOn


aftertouch      :: (Integral dt, Integral a, Integral b, Integral c) 
                => dt -> a -> b -> c -> Message
aftertouch      = voiceEventMsg3 NoteAftertouch
    

controller      :: (Integral dt, Integral a, Integral b, Integral c) 
                => dt -> a -> b -> c -> Message
controller      = voiceEventMsg3 Controller
    

prgm_change     :: (Integral dt, Integral a, Integral b) 
                => dt -> a -> b -> Message
prgm_change     = voiceEventMsg2 ProgramChange
    

chan_aftertouch :: (Integral dt, Integral a, Integral b) 
                => dt -> a -> b -> Message
chan_aftertouch = voiceEventMsg2 ChanAftertouch
    

pitchbend       :: (Integral dt, Integral a, Integral b) 
                => dt -> a -> b -> Message
pitchbend       = voiceEventMsg2 PitchBend
    

--------------------------------------------------------------------------------
   



generic_text    :: String -> Message
generic_text    = textEventMsg GENERIC_TEXT 

copyright       :: String -> Message
copyright       = textEventMsg COPYRIGHT_NOTICE 

seq_name        :: String -> Message
seq_name        = textEventMsg SEQUENCE_NAME 

inst_name       :: String -> Message
inst_name       = textEventMsg INSTRUMENT_NAME 

lyrics          :: String -> Message
lyrics          = textEventMsg LYRICS

marker          :: String -> Message
marker          = textEventMsg MARKER

cue_point       :: String -> Message
cue_point       = textEventMsg CUE_POINT


seq_number      :: Integral a => a -> Message
seq_number      = metaEventMsg SequenceNumber 0


chan_prefix     :: Integral a => a -> Message
chan_prefix     = metaEventMsg ChannelPrefix 0


end_of_track    :: Integral dt => dt -> Message
end_of_track dt = msg dt (MetaEvent EndOfTrack)


set_tempo       :: Integral a => a -> Message
set_tempo       = metaEventMsg SetTempo 0


smpte_offset    :: (Integral a, Integral b, Integral c, 
                    Integral d, Integral e) 
                => a -> b -> c -> d -> e -> Message
smpte_offset hr mn sc fr sf  = msgzero $ MetaEvent $ 
    SMPTEOffset (fromIntegral hr) (fromIntegral mn) (fromIntegral sc) 
                (fromIntegral fr) (fromIntegral sf)


time_sign       :: (Integral a, Integral b, Integral c, Integral d) 
                => a -> b -> c -> d -> Message
time_sign nr dr met n = msgzero $ MetaEvent $ 
    TimeSignature (fromIntegral dr) (fromIntegral nr) (fromIntegral met) 
                  (fromIntegral n)


key_sign        :: Integral a => a -> ScaleType -> Message
key_sign n t    = msgzero $ MetaEvent $ KeySignature (fromIntegral n) t



-- Nothing for SSME

 
majorScale          :: ScaleType
majorScale          = MAJOR 

minorScale          :: ScaleType
minorScale          = MINOR  

-- Major scales with sharps
c_major             :: Message
c_major             = key_sign 0 majorScale

g_major             :: Message
g_major             = key_sign 1 majorScale

d_major             :: Message
d_major             = key_sign 2 majorScale

a_major             :: Message
a_major             = key_sign 3 majorScale

e_major             :: Message
e_major             = key_sign 4 majorScale

b_major             :: Message
b_major             = key_sign 5 majorScale

f_sharp_major       :: Message
f_sharp_major       = key_sign 6 majorScale

c_sharp_major       :: Message
c_sharp_major       = key_sign 7 majorScale

-- Major scales with flats
f_major             :: Message
f_major             = key_sign (-1) majorScale

b_flat_major        :: Message
b_flat_major        = key_sign (-2) majorScale

e_flat_major        :: Message
e_flat_major        = key_sign (-3) majorScale

a_flat_major        :: Message
a_flat_major        = key_sign (-4) majorScale

d_flat_major        :: Message
d_flat_major        = key_sign (-5) majorScale

g_flat_major        :: Message
g_flat_major        = key_sign (-6) majorScale

c_flat_major        :: Message
c_flat_major        = key_sign (-7) majorScale



-- Minor scales with sharps
a_minor             :: Message
a_minor             = key_sign 0 minorScale

e_minor             :: Message
e_minor             = key_sign 1 minorScale

b_minor             :: Message
b_minor             = key_sign 2 minorScale

f_sharp_minor       :: Message 
f_sharp_minor       = key_sign 3 minorScale

c_sharp_minor       :: Message 
c_sharp_minor       = key_sign 4 minorScale

g_sharp_minor       :: Message 
g_sharp_minor       = key_sign 5 minorScale

d_sharp_minor       :: Message 
d_sharp_minor       = key_sign 6 minorScale

a_sharp_minor       :: Message 
a_sharp_minor       = key_sign 7 minorScale

-- Minor scales with flats
d_minor             :: Message
d_minor             = key_sign (-1) minorScale

g_minor             :: Message
g_minor             = key_sign (-2) minorScale

c_minor             :: Message
c_minor             = key_sign (-3) minorScale

f_minor             :: Message
f_minor             = key_sign (-4) minorScale

b_flat_minor        :: Message
b_flat_minor        = key_sign (-5) minorScale

e_flat_minor        :: Message
e_flat_minor        = key_sign (-6) minorScale

a_flat_minor        :: Message
a_flat_minor        = key_sign (-7) minorScale


