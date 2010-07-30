{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Pretty
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Pretty print the MIDI representation.
--
--------------------------------------------------------------------------------


module ZMidi.Core.Pretty
  (

    printMidi

  , header
  , track

  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.SimpleFormat

import Data.List
import Data.Monoid
import Data.Word



printMidi :: MidiFile -> IO ()
printMidi (MidiFile hdr tracks) = do
    column_break
    mapM_ putStrLn (header hdr)  
    mapM_ (\t -> column_break >> putTrack t) tracks
  where
    putTrack       = (mapM_ putStrLn) . track
    column_break   = putStrLn $ replicate 60 '-'

header :: Header -> [String]
header (Header fmt tcount td) = 
   map output [ppFormat fmt, ppNumTracks tcount, ppTimeDivision td]  


track  :: Track -> [String]
track = snd . mapAccumL (\acc b -> msnd output $ message acc b) 0 . getMessages 
  where
    msnd f (a,b) = (a,f b)

--------------------------------------------------------------------------------

column2 :: String -> Doc -> Doc
column2 s d2 = padr 20 (text s) `sep` char '|' `ssep` d2 

ppFormat :: Format -> Doc
ppFormat = column2 "MIDI Format" . step 
  where
    step MF0  = text "Type 0 MIDI File"
    step MF1  = text "Type 1 MIDI File"
    step MF2  = text "Type 2 MIDI File"

ppNumTracks :: Word16 -> Doc
ppNumTracks = column2 "Number of tracks" . integral

ppTimeDivision :: TimeDivision -> Doc
ppTimeDivision = column2 "Time Division" . step
  where
    step (FPS i)   = text "fps"   `ssep` integral i
    step (TPB i)   = text "ticks" `ssep` integral i

infixr 7 `dashsep`

dashsep :: Doc -> Doc -> Doc
dashsep d1 d2 = d1 `ssep` char '-' `ssep` d2

message :: Word32 -> Message -> (Word32,Doc)
message acc (delta,evt) = 
    (n, acctime `dashsep` dtime `dashsep` ppEvent evt)
  where
    n             = acc + delta 
    acctime = padl 12 (integral n)
    dtime   = padl 6  (integral delta)

ppEvent :: Event -> Doc
ppEvent (DataEvent e)         = ppDataEvent e
ppEvent (VoiceEvent e)        = ppVoiceEvent e
ppEvent (SysExEvent e)        = ppSysExEvent e
ppEvent (SysCommonEvent e)    = ppSysCommonEvent e
ppEvent (SysRealTimeEvent e)  = ppSysRealTimeEvent e
ppEvent (MetaEvent e)         = ppMetaEvent e


event :: String -> Doc -> Doc
event s d = padr 18 (text s) `dashsep` d

ppDataEvent :: DataEvent -> Doc
ppDataEvent (Data1 tag)       = event "data" (hex2 tag)

ppVoiceEvent :: VoiceEvent -> Doc
ppVoiceEvent (Controller c n v)     = 
    event "controller" (hex2 c `ssep` hex2 n `ssep` hex2 v)

ppVoiceEvent (ProgramChange c n)    = 
    event "program-change" (hex2 c `ssep` hex2 n)

ppVoiceEvent (NoteOff c n v)        =
    event "note-off" (hex2 c `ssep` hex2 n `ssep` hex2 v)

ppVoiceEvent (NoteOn c n v)         = 
    event "note-on" (hex2 c `ssep` hex2 n `ssep` hex2 v)

ppVoiceEvent (NoteAftertouch c n v) =
    event "note-aftertouch" (hex2 c `ssep` hex2 n `ssep` hex2 v)

ppVoiceEvent (ChanAftertouch c v)   = 
    event "channel-aftertouch" (hex2 c `ssep` hex2 v)

ppVoiceEvent (PitchBend c v)        =
    event "pitch-bend" (hex2 c `ssep` hex4 v)


ppSysExEvent :: SysExEvent -> Doc
ppSysExEvent (SysEx n ws) = event "sys-ex" $ byteList n ws

ppSysCommonEvent :: SysCommonEvent -> Doc
ppSysCommonEvent (QuarterFrame sb)      = 
    event "time-code-quarter-frame" (hex2 $ joinByte sb)

ppSysCommonEvent (SongPosPointer a b)   = 
    event "sys-common song pos. pointer" (hex2 a `ssep` hex2 b)

ppSysCommonEvent (SongSelect w)         = event "song-select" (hex2 w)

ppSysCommonEvent (Common_undefined tag) = event "sys-common" (hex2 tag)

ppSysCommonEvent TuneRequest            = text "tune-request"

ppSysCommonEvent EOX                    = text "end-of-sys-ex"


ppSysRealTimeEvent :: SysRealTimeEvent -> Doc
ppSysRealTimeEvent TimingClock          = text "sys-real-time timing-clock"
ppSysRealTimeEvent (RT_undefined tag)   = event "sys-real-time" (hex2 tag)
ppSysRealTimeEvent StartSequence        = text "sys-real-time start"
ppSysRealTimeEvent ContinueSequence     = text "sys-real-time continue"
ppSysRealTimeEvent StopSequence         = text "sys-real-time stop"
ppSysRealTimeEvent ActiveSensing        = text "sys-real-time active sensing"
ppSysRealTimeEvent SystemReset          = text "system-reset"

ppMetaEvent :: MetaEvent -> Doc
ppMetaEvent (TextEvent ty s)          = event (textType ty) (text s)

ppMetaEvent (SequenceNumber w)        = event "sequence-number" (hex4 w)

ppMetaEvent (ChannelPrefix a b)       = 
    event "channel-prefix" (hex2 a `ssep` hex2 b)

ppMetaEvent EndOfTrack                = text "end-of-track"

ppMetaEvent (SetTempo w)              = event "set-tempo" (integral w) 

ppMetaEvent (SMPTEOffset h m s f sf)  = 
    event "smpte-offest" (mconcat $ map hex2 [h,m,s,f,sf])

ppMetaEvent (TimeSignature n d m t)   = 
    event "time-signature" (mconcat $ map hex2 [n,d,m,t])

ppMetaEvent (KeySignature n sc)       = 
    event "key-signature" (integral n `ssep` ppScale sc)

ppMetaEvent (SSME n ws)               = 
    event "sequencer-specific" (byteList n ws)

byteList :: Integral a => a -> [Word8] -> Doc 
byteList n ws | n < 10    = integral n `sep` mconcat (map hex2 ws)
              | otherwise = integral n `sep` multiply 10 '.'


textType :: TextType -> String
textType GENERIC_TEXT         =  "generic-text" 
textType COPYRIGHT_NOTICE     =  "copyright-notice"  
textType SEQUENCE_NAME        =  "sequence-name"
textType INSTRUMENT_NAME      =  "instrument-name"
textType LYRICS               =  "lyrics"
textType MARKER               =  "marker"
textType CUE_POINT            =  "cue-point"
  

ppScale :: ScaleType -> Doc
ppScale MAJOR  = text "major"
ppScale MINOR  = text "minor"