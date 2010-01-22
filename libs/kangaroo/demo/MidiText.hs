{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  MidiText
-- Copyright   :  (c) Stephen Tetley 2009-2010
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
    -- * Print a midi file to stdout
    printMidi

  ) where


import MidiDatatypes
import Text.PrettyPrint.JoinPrint hiding ( length )


import qualified Data.Foldable as F
import Data.Word

void :: Monad m => m a -> m ()
void mf = mf >> return ()


printMidi :: MidiFile -> IO ()
printMidi (MidiFile hdr tracks)  = do 
   header hdr 
   void $ F.foldlM (\i t -> track i t >> return (i+1)) 0 tracks


header :: Header -> IO () 
header (Header hformat ntrks td) = renderIO $ 
    punctuate (text " - ") [ text "[Header]" <+> ppHFormat hformat
                           , integral ntrks <+> text "tracks"
                           , ppTimeDivision td
                           ]
  
ppHFormat :: HFormat -> Doc
ppHFormat MF0  = text "Type 0"
ppHFormat MF1  = text "Type 1"
ppHFormat MF2  = text "Type 2"
 
ppTimeDivision :: TimeDivision -> Doc
ppTimeDivision (FPS i)   = text "fps"   <+> integral i
ppTimeDivision (TPB i)   = text "ticks" <+> integral i

track :: Int -> Track -> IO ()
track i (Track se) = do 
    renderIO $ brackets (text "Track" <+> int i) 
    void $ F.foldlM fn 0 se
  where
    fn gt (dt,evt)  = let (name,d) =  event evt in
                      do { renderIO $ mkEvent gt dt name d 
                         ; return (gt+dt)
                         }


mkEvent :: Word32 -> DeltaTime -> String -> Doc -> Doc
mkEvent ot dt name rest = let s = text " - " in
    punctuate s [ padl 10 ' ' (integral ot)
                , padl 6  ' ' (integral dt)
                , padr 20 ' ' (text name)
                , rest]

type EventDescr = (String,Doc)


event :: Event -> EventDescr
event (VoiceEvent e)   = voiceEvent e
event (SystemEvent e)  = let (s,d) = systemEvent e in ("sys " ++s, d)
event (MetaEvent e)    = let (s,d) = metaEvent e in ("meta " ++ s, d)


fli :: (Integral a, Show a) => a -> Doc
fli = padl 3 ' ' . integral

voiceEvent :: VoiceEvent -> EventDescr
voiceEvent (NoteOff ch nt vel)        = ("note-off",    dashInts [ch,nt,vel])
voiceEvent (NoteOn ch nt vel)         = ("note-on",     dashInts [ch,nt,vel])
voiceEvent (NoteAftertouch ch nt val) = ("note-after-touch", dashInts [ch,nt,val])
voiceEvent (Controller ch ty val)     = ("ctlr",        dashInts [ch,ty,val])
voiceEvent (ProgramChange ch num)     = ("pc",          dashInts [ch,num])
voiceEvent (ChanAftertouch ch val)    = ("chan-after-touch", dashInts [ch,val])
voiceEvent (PitchBend ch val)         = ("pitch-bend",  dashInts [fromIntegral ch,val])

  

systemEvent :: SystemEvent -> EventDescr
systemEvent (SysEx _ _)    = ("sysex", empty)   
systemEvent (DataEvent i)  = ("data",  oxhex2 i)

metaEvent :: MetaEvent -> EventDescr
metaEvent (TextEvent ty s)          = (textType ty, dquotes $ text s)
metaEvent (SequenceNumber i)        = ("sequence-number", integral i)
metaEvent (ChannelPrefix ch)        = ("channel-prefix",  integral ch)
metaEvent (EndOfTrack)              = ("end-of-track", empty)
metaEvent (SetTempo mspqn)          = ("set-tempo", integral mspqn)
metaEvent (SMPTEOffset h m s f sf)  = ("smpte",     dashInts [h,m,s,f,sf])
metaEvent (TimeSignature n d m ns)  = ("time-sig",  dashInts [n,d,m,ns])
metaEvent (KeySignature i sc)       = ("key-sig", integral i `hyph` st) 
     where st = text $ scaleType sc
metaEvent (SSME i _)                = ("ssme",    oxhex8 i <+> text "...")

dashInts :: Integral a => [a] -> Doc
dashInts = punctuate (text " - ") . map fli


scaleType :: ScaleType -> String
scaleType MAJOR  = "major"
scaleType MINOR  = "minor"
  
textType :: TextType -> String
textType GENERIC_TEXT         =  "generic-text" 
textType COPYRIGHT_NOTICE     =  "copyright-notice"  
textType SEQUENCE_NAME        =  "sequence-name"
textType INSTRUMENT_NAME      =  "instrument-name"
textType LYRICS               =  "lyrics"
textType MARKER               =  "marker"
textType CUE_POINT            =  "cue-point"
  

infixl 6 `hyph`

hyph :: Doc -> Doc -> Doc
x `hyph` y         = x <> text " - " <> y

