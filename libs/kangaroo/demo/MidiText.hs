{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  MidiText
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  volatile
-- Portability :  to be determined.
--
-- Pretty print a textual representaton of MIDI files. 
--
--------------------------------------------------------------------------------


module MidiText (
    -- * Print a readable text representation
    printMidi, ppMidiFile,
    
    ppHeader, ppMessage,
  
  ) where


import MidiDatatypes
import Text.PrettyPrint.JoinPrint


import qualified Data.Foldable as F
import Numeric


printMidi :: MidiFile -> IO ()
printMidi = putStrLn . render . ppMidiFile 




integral :: Integral a => a -> Doc
integral = integer . fromIntegral

infixl 6 `hyph`, `dblhyph`

hyph :: Doc -> Doc -> Doc
x `hyph` y         = x <> text " - " <> y

dblhyph :: Doc -> Doc -> Doc
x `dblhyph` y         = x <> text " -- " <> y

ppHex :: Integral a => a -> Doc
ppHex i = text $ showHex i []



ppMidiFile :: MidiFile -> Doc
ppMidiFile (MidiFile header tracks)  = ppHeader header <%> tdoc <%> empty
  where
    tdoc = fst $ F.foldl fn (empty,0) tracks
    fn (d,i) t = (d <%> prettyTrack t i, i+1)
      
ppHeader :: Header -> Doc
ppHeader (Header hformat ntrks td) = 
    (text "[Header]" <+> ppHFormat hformat)
                    `hyph` (integral ntrks <+> text "tracks") 
                    `hyph` ppTimeDivision td
  
ppHFormat :: HFormat -> Doc
ppHFormat MF0  = text "Type 0"
ppHFormat MF1  = text "Type 1"
ppHFormat MF2  = text "Type 2"
 
ppTimeDivision :: TimeDivision -> Doc
ppTimeDivision (FPS i)   = text "fps"   <+> integral i
ppTimeDivision (TPB i)   = text "ticks" <+> integral i

prettyTrack :: Track -> Int -> Doc
prettyTrack (Track se) i = 
    brackets (text "Track" <+> int i) <%> (snd $ F.foldl fn (0,empty) se)
  where
    fn (gt,doc) msg@(dt,_)  = 
      (gt+dt, doc <%> (padshow 8 (gt+dt) `hyph` ppMessage msg))    


ppMessage :: Message -> Doc
ppMessage (dt,evt) = padshow 5 dt `hyph` ppEvent evt      


ppEvent :: Event -> Doc
ppEvent (VoiceEvent e)   = ppVoiceEvent e
ppEvent (SystemEvent e)  = text "sys"  `dblhyph` ppSystemEvent e
ppEvent (MetaEvent e)    = text "meta" `dblhyph` ppMetaEvent e

padshow :: Show a => Int -> a -> Doc
padshow pad a = let s = show a ; dif = pad - length s in
    text $ replicate dif ' ' ++ s 
                  


fli :: (Integral a, Show a) => a -> Doc
fli = padshow 3

ppVoiceEvent :: VoiceEvent -> Doc
ppVoiceEvent (NoteOff ch nt vel)        = 
    text "note-off" `hyph` fli ch `hyph` fli nt `hyph` fli vel
 
ppVoiceEvent (NoteOn ch nt vel)         =
    text "note-on " `hyph` fli ch `hyph` fli nt `hyph` fli vel
    
ppVoiceEvent (NoteAftertouch ch nt val) = 
    text "note-after-touch" `hyph` fli ch `hyph` fli nt `hyph` fli val
    
ppVoiceEvent (Controller ch ty val)     = 
    text "ctlr" `hyph` fli ch `hyph` fli ty `hyph` fli val
    
ppVoiceEvent (ProgramChange ch num)     = 
    text "pc" `hyph` fli ch `hyph` fli num
    
ppVoiceEvent (ChanAftertouch ch val)    = 
    text "chan-after-touch" `hyph` fli ch `hyph` fli val
      
ppVoiceEvent (PitchBend ch val)         = 
    text "pitch-bend" `hyph` fli ch `hyph` fli val

  

ppSystemEvent :: SystemEvent -> Doc
ppSystemEvent (SysEx _ _)    = text "sysex"   -- system exclusive event - length x data
ppSystemEvent (DataEvent i)  = text "data" <+> ppHex i

ppMetaEvent :: MetaEvent -> Doc
ppMetaEvent (TextEvent ty s)     = ppTextType ty `hyph` dquotes (text s)
ppMetaEvent (SequenceNumber i)   = text "sequence-number" `hyph` integral i
ppMetaEvent (ChannelPrefix ch)   = text "channel-prefix"  `hyph` integral ch
ppMetaEvent (EndOfTrack)         = text "end-of-track"
ppMetaEvent (SetTempo mspqn)     = text "set-tempo" `hyph` integral mspqn  
                                        -- microseconds per quarter-note
ppMetaEvent (SMPTEOffset h m s f sf) = 
    text "smpte" `hyph` fli h `hyph` fli m `hyph` fli s `hyph` fli f `hyph` fli sf
    
ppMetaEvent (TimeSignature n d m ns) = 
    text "time-sig" `hyph` fli n `hyph` fli d `hyph` fli m `hyph` fli ns
    
ppMetaEvent (KeySignature i sc)  = 
    text "key-sig" `hyph` integral i `hyph` ppScaleType sc
       
ppMetaEvent (SSME i _)           = text "ssme" `hyph` (ppHex i <+> text "...")

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
  
