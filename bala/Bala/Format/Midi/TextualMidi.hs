
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Midi.TextualMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A textual representaton of MIDI files 
-- |
--------------------------------------------------------------------------------


module Bala.Format.Midi.TextualMidi where

import Bala hiding (int,integer)
import Bala.Format.Midi.Datatypes


import Data.Bits
import Data.Char
import Data.Tuple
import Data.Word
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
  
--------------------------------------------------------------------------------
-- `binary text`
--------------------------------------------------------------------------------


bchar :: Char -> ShowS
bchar c | isPrint c == True = showChar c
        | otherwise         = showDot

bword :: Word8 -> ShowS
bword i | i < 16    = showChar '0' . showHex i
        | otherwise = showHex i


word16be :: Word16 -> (Word8,Word8)
word16be i = (a,b)
  where a = fromIntegral $ 0xff .&. i `shiftR` 8
        b = fromIntegral $ 0xff .&. i 
         
word32be :: Word32 -> (Word8,Word8,Word8,Word8)
word32be i = (a,b,c,d)
  where a = fromIntegral $ 0xff .&. i `shiftR` 24
        b = fromIntegral $ 0xff .&. i `shiftR` 16
        c = fromIntegral $ 0xff .&. i `shiftR` 8
        d = fromIntegral $ 0xff .&. i      
        
class ByteShow a where 
  byteChar :: a -> ShowS
  byteHex  :: a -> ShowS

instance ByteShow Char where
  byteChar = bchar
  byteHex  = bword . fromIntegral . ord

 
instance ByteShow Word8 where
  byteChar = byteChar . chr . fromIntegral
  byteHex  = bword 

instance ByteShow Word16 where
  byteChar i = let (a,b) = word16be i in byteChar a . byteChar b
  byteHex  i = let (a,b) = word16be i in byteHex a . showSpace . byteHex b
  
instance ByteShow Word32 where
  byteChar i = let (a,b,c,d) = word32be i in 
               byteChar a . byteChar b . byteChar c . byteChar d
  byteHex  i = let (a,b,c,d) = word32be i in 
               byteHex a . showSpace . byteHex b . showSpace . 
               byteHex c . showSpace . byteHex d

instance ByteShow a => ByteShow [a] where
  byteChar []     = id
  byteChar (c:cs) = foldl (\acc a -> acc . byteChar a) (byteChar c) cs
  
  byteHex []      = id
  byteHex (c:cs)  = foldl (\acc a -> acc . showSpace . byteHex a) (byteHex c) cs
  

bytestr :: String -> ShowS
bytestr (c:cs) = bchar c . bytestr cs
bytestr []     = id

strhex []     = id
strhex (c:cs) = foldl (\acc a -> acc . showSpace . byteHex a) (byteHex c) cs


brep1 :: (ByteShow a, Show a) => (a -> ShowS) -> a -> ShowS
brep1 f a = byteHex a . showSpace . withParens (f a)

brepStr = brep1 showString
brepChr = brep1 showChar


-- incorrect
brepVarlen :: Word32 -> ShowS
brepVarlen 0 = bword 0 . showSpace . withParens (shows 0)
brepVarlen a = (byteHex $ varlenSplit a) . showSpace . withParens (shows a) . showString " -- check"


demo = brep1 shows (1000::Word32) []

