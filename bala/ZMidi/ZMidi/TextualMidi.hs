
--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.TextualMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print a textual representaton of MIDI files. 
--
--------------------------------------------------------------------------------


module ZMidi.TextualMidi (
    -- * Print a readable text representation
    printMidi, ppMidiFile,
    
--    brep1, brepVarlen, brepStr
  
  ) where


import ZMidi.Datatypes


import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import Data.Tuple
import Data.Word
import Numeric
import Text.PrettyPrint.HughesPJ


printMidi :: MidiFile -> IO ()
printMidi = putStrLn . render . ppMidiFile 




integral :: Integral a => a -> Doc
integral = integer . fromIntegral

infixl 6 .-., .--.

(.-.) :: Doc -> Doc -> Doc
x .-. y         = x <> text " - " <> y

(.--.) :: Doc -> Doc -> Doc
x .--. y         = x <> text " -- " <> y

ppHex :: Integral a => a -> Doc
ppHex i = text $ showHex i []

mazi :: ((a,Int) -> b) -> Int -> [a] -> [b]
mazi f i xs = map f (zip xs [i..]) 


ppMidiFile :: MidiFile -> Doc
ppMidiFile (MidiFile header tracks)  = ppHeader header $$ tdoc $$ empty
  where
    tdoc = fst $ F.foldl fn (empty,0) tracks
    fn (d,i) t = (d $$ prettyTrack t i, i+1)
      
ppHeader :: Header -> Doc
ppHeader (Header hformat ntrks td) = 
    text "[Header]" <+> ppHFormat hformat 
                    .-. (integral ntrks <+> text "tracks") 
                    .-. ppTimeDivision td
  
ppHFormat :: HFormat -> Doc
ppHFormat MF0  = text "Type 0"
ppHFormat MF1  = text "Type 1"
ppHFormat MF2  = text "Type 2"
 
ppTimeDivision :: TimeDivision -> Doc
ppTimeDivision (FPS i)   = text "fps" <+> integral i
ppTimeDivision (TPB i)   = text "ticks" <+> integral i


prettyTrack (Track se) i = 
    brackets (text "Track" <+> int i) $$ (snd $ F.foldl fn (0,empty) se)
  where
    fn (gt,doc) msg@(dt,_)  = 
      (gt+dt, doc $$ (padshow 8 (gt+dt) .-. ppMessage msg))    


ppMessage :: Message -> Doc
ppMessage (dt,evt) = padshow 5 dt .-. ppEvent evt      


ppEvent :: Event -> Doc
ppEvent (VoiceEvent e)   = ppVoiceEvent e
ppEvent (SystemEvent e)  = text "sys"  .--. ppSystemEvent e
ppEvent (MetaEvent e)    = text "meta" .--. ppMetaEvent e

padshow :: Show a => Int -> a -> Doc
padshow pad a = let s = show a ; dif = pad - length s in
    text $ replicate dif ' ' ++ s 
                  


fli :: (Integral a, Show a) => a -> Doc
fli = padshow 3

ppVoiceEvent :: VoiceEvent -> Doc
ppVoiceEvent (NoteOff ch nt vel)        = 
    text "note-off" .-. fli ch .-. fli nt .-. fli vel
 
ppVoiceEvent (NoteOn ch nt vel)         =
    text "note-on " .-. fli ch .-. fli nt .-. fli vel
    
ppVoiceEvent (NoteAftertouch ch nt val) = 
    text "note-after-touch" .-. fli ch .-. fli nt .-. fli val
    
ppVoiceEvent (Controller ch ty val)     = 
    text "ctlr" .-. fli ch .-. fli ty .-. fli val
    
ppVoiceEvent (ProgramChange ch num)     = 
    text "pc" .-. fli ch .-. fli num
    
ppVoiceEvent (ChanAftertouch ch val)    = 
    text "chan-after-touch" .-. fli ch .-. fli val
      
ppVoiceEvent (PitchBend ch val)         = 
    text "pitch-bend" .-. fli ch .-. fli val

  

ppSystemEvent :: SystemEvent -> Doc
ppSystemEvent (SysEx _ _)    = text "sysex"   -- system exclusive event - length x data
ppSystemEvent (DataEvent i)  = text "data" <+> ppHex i

ppMetaEvent :: MetaEvent -> Doc
ppMetaEvent (TextEvent ty s)     = ppTextType ty .-. doubleQuotes (text s)
ppMetaEvent (SequenceNumber i)   = text "sequence-number" .-. integral i
ppMetaEvent (ChannelPrefix ch)   = text "channel-prefix"  .-. integral ch
ppMetaEvent (EndOfTrack)         = text "end-of-track"
ppMetaEvent (SetTempo mspqn)     = text "set-tempo" .-. integral mspqn  -- microseconds per quarter-note
ppMetaEvent (SMPTEOffset h m s f sf) = 
    text "smpte" .-. fli h .-. fli m .-. fli s .-. fli f .-. fli sf
    
ppMetaEvent (TimeSignature n d m ns) = 
    text "time-sig" .-. fli n .-. fli d .-. fli m .-. fli ns
    
ppMetaEvent (KeySignature i sc)  = 
    text "key-sig" .-. integral i .-. ppScaleType sc
       
ppMetaEvent (SSME i _)           = text "ssme" .-. ppHex i <+> text "..."

ppScaleType :: ScaleType -> Doc
ppScaleType MAJOR  = text "major"
ppScaleType MINOR  = text "minor"
  
ppTextType :: TextType -> Doc
ppTextType GENERIC_TEXT         = text "generic-text" 
ppTextType COPYRIGHT_NOTICE     = text "copyright-notice"  
ppTextType SEQUENCE_NAME        = text "sequence-name"
ppTextType INSTRUMENT_NAME      = text "instrument-name"
ppTextType LYRICS               = text "lyrics"
ppTextType MARKER               = text "marker"
ppTextType CUE_POINT            = text "cue-point"
  
