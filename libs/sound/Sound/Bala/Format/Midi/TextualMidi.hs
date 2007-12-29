
module Sound.Bala.Format.Midi.TextualMidi where

import Sound.Bala.Format.Midi.Datatypes

import Data.Tuple
import Numeric
import Text.PrettyPrint.Leijen


ppMidiFile :: MidiFile -> Doc
ppMidiFile = pretty

integral :: Integral a => a -> Doc
integral = integer . fromIntegral

infixr 6 <->, <-->

(<->) :: Doc -> Doc -> Doc
x <-> y         = x <> text " - " <> y

(<-->) :: Doc -> Doc -> Doc
x <--> y         = x <> text " -- " <> y

ppHex :: Integral a => a -> Doc
ppHex i = text $ showHex i []

mazi :: ((a,Int) -> b) -> Int -> [a] -> [b]
mazi f i xs = map f (zip xs [i..]) 


instance Pretty MidiFile where
  pretty (MidiFile header tracks)  = 
    pretty header <$> vcat (mazi (uncurry prettyTrack) 0 tracks) <$> empty

instance Pretty Header where
  pretty (Header hformat ntrks td) = 
    text "[Header]" <+> pretty hformat 
                    <-> (integral ntrks <+> text "tracks") 
                    <-> pretty td
  
instance Pretty HFormat where
  pretty MF0  = text "Type 0"
  pretty MF1  = text "Type 1"
  pretty MF2  = text "Type 2"
 
instance Pretty TimeDivision where
  pretty (FPS i)   = text "fps" <+> integral i
  pretty (TPB i)   = text "ticks" <+> integral i


prettyTrack (Track messages) i = 
  brackets (text "Track" <+> int i) <$> vcat (map prettyMessage messages) 

prettyMessage (dt,evt) = fill 5 (integral dt) <-> pretty evt      


instance Pretty Event where
  pretty (VoiceEvent e)   = pretty e
  pretty (SystemEvent e)  = text "sys" <--> pretty e
  pretty (MetaEvent e)    = text "meta" <--> pretty e

fli :: Integral a => a -> Doc
fli = fill 3 . integral

instance Pretty VoiceEvent where
  pretty (NoteOff ch nt vel) = 
    text "note-off" <-> fli ch <-> fli nt <-> fli vel
 
  pretty (NoteOn ch nt vel) =
    text "note-on " <-> fli ch <-> fli nt <-> fli vel
    
  pretty (NoteAftertouch ch nt val) = 
    text "note-after-touch" <-> fli ch <-> fli nt <-> fli val
    
  pretty (Controller ch ty val) = 
    text "ctlr" <-> fli ch <-> fli ty <-> fli val
    
  pretty (ProgramChange ch num) = 
    text "pc" <-> fli ch <-> fli num
    
  pretty (ChanAftertouch ch val) = 
    text "chan-after-touch" <-> fli ch <-> fli val
      
  pretty (PitchBend ch val) = 
    text "pitch-bend" <-> fli ch <-> fli val

  

instance Pretty SystemEvent where
  pretty (SysEx _ _)    = text "sysex"   -- ^ system exclusive event - length x data               
  pretty (DataEvent i)  = text "data" <+> ppHex i

instance Pretty MetaEvent where
  pretty (TextEvent ty s)     = pretty ty <-> dquotes (text s)
  pretty (SequenceNumber i)   = text "sequence-number" <-> integral i
  pretty (ChannelPrefix ch)   = text "channel-prefix"  <-> integral ch
  pretty (EndOfTrack)         = text "end-of-track"
  pretty (SetTempo mspqn)     = text "set-tempo" <-> integral mspqn              -- ^ microseconds per quarter-note
  pretty (SMPTEOffset h m s f sf) = 
    text "smpte" <-> fli h <-> fli m <-> fli s <-> fli f <-> fli sf
  pretty (TimeSignature n d m ns) = 
    text "time-sig" <-> fli n <-> fli d <-> fli m <-> fli ns
  pretty (KeySignature i sc)  = text "key-sig" <-> integral i <-> pretty sc   
  pretty (SSME i _)           = text "ssme" <-> ppHex i <+> text "..."

instance Pretty Scale where
  pretty MAJOR  = text "major"
  pretty MINOR  = text "minor"
  
instance Pretty TextType where
  pretty GENERIC_TEXT         = text "generic-text" 
  pretty COPYRIGHT_NOTICE     = text "copyright-notice"  
  pretty SEQUENCE_NAME        = text "sequence-name"
  pretty INSTRUMENT_NAME      = text "instrument-name"
  pretty LYRICS               = text "lyrics"
  pretty MARKER               = text "marker"
  pretty CUE_POINT            = text "cue-point"
  
