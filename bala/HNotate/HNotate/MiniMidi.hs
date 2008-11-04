--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.MiniMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A tiny Midi output module
--
--------------------------------------------------------------------------------

module HNotate.MiniMidi where


import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char
import qualified Data.Foldable as F
import Data.Int
import Data.Sequence hiding (length)
import qualified Data.Sequence as S
import Data.Word

import System.IO

ticks_per_beat :: Word16
ticks_per_beat = 384


midi_double_whole   :: Word32
midi_double_whole   = midi_whole * 2

midi_whole          :: Word32
midi_whole          = midi_half * 2

midi_half           :: Word32
midi_half           = midi_quarter * 2

midi_quarter        :: Word32
midi_quarter        = 384

midi_eighth         :: Word32
midi_eighth         = 192

midi_sixteenth      :: Word32
midi_sixteenth      = 96

midi_thirty_second  :: Word32
midi_thirty_second  = 48

midi_sixty_fourth   :: Word32
midi_sixty_fourth   = 24



-------------------------------------------------------------------------------
-- Datatypes

-- only consider type1 files & ticks per beat is always 384
data MidiFile = MidiFile { midi_tracks          :: Seq Track }
  deriving (Eq,Show)
  
newtype Track = Track { getTrack :: Seq Message }
  deriving (Eq,Show)

    
type DeltaTime = Word32
  
newtype Message = Message { getMessage :: (DeltaTime, Event) }
  deriving (Eq,Show) 

-- This is useful for rendering - of we have have a NoteOn and a NoteOff
-- on the same channel at the same time we want the NoteOff played first.

instance Ord Message where
    compare (Message (t,e)) (Message (t',e')) = (t,e) `compare` (t',e')


-- Note - the constructors are ordered for rendering and not by midi event
-- code. When rendering we always want MetaSeqNum first in the track, NoteOffs
-- should be before NoteOns...
data Event = MetaSeqNum   Word16
           | MetaText     TextType String
           | MetaSetTempo Word32
           | MetaTimeSig  Word8 Word8 Word8 Word8 -- num den metro num32
           | MetaKeySig   Int8 ScaleType 
           | VoiceNoteOff Word8 Word8 Word8   -- chan note velocity
           | VoiceNoteOn  Word8 Word8 Word8   -- chan note velocity
           | MetaEOT
  deriving (Eq,Ord,Show)
  
data TextType = GENERIC_TEXT 
              | COPYRIGHT_NOTICE 
              | SEQUENCE_NAME 
              | INSTRUMENT_NAME
              | LYRICS 
              | MARKER 
              | CUE_POINT 
  deriving (Eq,Enum,Ord,Show) 
  
  
data ScaleType = MAJOR | MINOR
  deriving (Eq,Enum,Ord,Show)

--------------------------------------------------------------------------------
-- Some shorthand constructors and message instances

controlTrack :: Word32 -> Track
controlTrack tempo = let t_msg = Message (0, MetaSetTempo tempo)
                     in Track (empty |> t_msg |> eot_msg) 

eot_msg :: Message
eot_msg = Message (0,    MetaEOT)


--------------------------------------------------------------------------------
-- Write File

type MidiBlock = B.ByteString

type MidiOut = MidiBlock -> MidiBlock

infixr 5 `u4l4`

u4l4 :: Word8 -> Word8 -> Word8
a `u4l4` b = (a `shiftL` 4) + b 

out1 :: Word8 -> (B.ByteString -> B.ByteString)
out1 = B.cons

out2 :: Word8 -> Word8 -> (B.ByteString -> B.ByteString)
out2 a b = (B.cons a) . (B.cons b) 

out3 :: Word8 -> Word8 -> Word8 -> (B.ByteString -> B.ByteString)
out3 a b c = (B.cons a) . (B.cons b) . (B.cons c)

out4 :: Word8 -> Word8 -> Word8 -> Word8 -> (B.ByteString -> B.ByteString)
out4 a b c d = (B.cons a) . (B.cons b) . (B.cons c) . (B.cons d)

out5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 
     ->(B.ByteString -> B.ByteString)
out5 a b c d e = (B.cons a) . (B.cons b) . (B.cons c) . (B.cons d) . (B.cons d)


writeMidiFile :: FilePath -> MidiFile -> IO ()
writeMidiFile path midi = let midistream = outputMidiFileS midi $ B.empty in do
    h <- openBinaryFile path WriteMode
    B.hPut h midistream
    hClose h

  
outputMidiFileS :: MidiFile -> MidiOut
outputMidiFileS (MidiFile se) = outputHeaderS (S.length se) . fns
  where fns = F.foldr (\a f -> outputTrackS a . f) id se


outputHeaderS :: Int -> MidiOut
outputHeaderS num_tracks = 
    outString "MThd" . outW32 6 . out2 0 1 . ftracks . ftpb
  where
    ftracks = outW16 $ fromIntegral num_tracks
    ftpb    = outW16 $ ticks_per_beat `clearBit` 15
    
    
outputTrackS :: Track -> MidiOut
outputTrackS (Track se) = 
    let fn = F.foldr (\a f -> outputMessageS a . f) id se
        bs = fn B.empty
    in outString "MTrk" . (outW32 $ fromIntegral $ B.length bs) . (B.append bs)      

outputMessageS :: Message -> MidiOut
outputMessageS (Message (dt,evt)) = varlen dt . outputEventS evt 

outputEventS :: Event -> MidiOut
outputEventS (MetaSeqNum n)           = out3 0xFF 0x00 2 . outW16 n
outputEventS (MetaText tt s)          = 
    out2 0xFF (texttype tt) . (varlen $ fromIntegral $ length s) . outString s
      
outputEventS (MetaSetTempo t)         = out3 0xFF 0x51 3 . outW24 t
outputEventS (MetaTimeSig  n d m n32) = out3 0xFF 0x58 4 . out4 n d m n32 
outputEventS (MetaKeySig i sc)        = 
    out3 0xFF 0x59 2 . out2 (wrapint i) (wscale sc)
    
outputEventS (VoiceNoteOff ch n v)    = out3 (0x8 `u4l4` ch) n v
outputEventS (VoiceNoteOn ch n v)     = out3 (0x9 `u4l4` ch) n v    
outputEventS MetaEOT                  = out3 0xFF 0x2F 0
   

wrapint :: Int8 -> Word8
wrapint i | i < 0     = fromIntegral $ i' + 256
          | otherwise = fromIntegral i
  where
    i' :: Int
    i' = fromIntegral i  
    
wscale :: ScaleType -> Word8
wscale MAJOR = 0x00
wscale MINOR = 0x01

outW16 :: Word16 -> MidiOut
outW16 i = out2 b a 
  where 
  (a, r1)   = lowerEight i     
  (b, _)    = lowerEight r1

outW24 :: Word32 -> MidiOut
outW24 i = out3 c b a 
  where 
  (a, r1)   = lowerEight i     
  (b, r2)   = lowerEight r1
  (c, _)    = lowerEight r2
  
outW32 :: Word32 -> MidiOut
outW32 i = out4 d c b a 
  where 
  (a, r1)   = lowerEight i     
  (b, r2)   = lowerEight r1
  (c, r3)   = lowerEight r2
  (d, _)    = lowerEight r3
    

    

lowerEight :: (Bits a, Integral a) => a -> (Word8, a)    
lowerEight n = (fromIntegral lower8, remain)
  where
    remain = n `shiftR` 8
    lower8 = n .&. 0xff 
      
varlen :: Word32 -> MidiOut
varlen i  
    | i < 0x80        = out1 (fromIntegral i)
    | i < 0x4000      = out2 (wise i 7)  (wise i 0)
    | i < 0x200000    = out3 (wise i 14) (wise i 7)  (wise i 0) 
    | otherwise       = out4 (wise i 21) (wise i 14) (wise i 7) (wise i 0) 
  where         
    wise i 0 = fromIntegral $ i .&. 0x7F
    wise i n = fromIntegral $ i `shiftR` n   .&.  0x7F  .|.  0x80;

outString :: String -> MidiOut    
outString s = B.append (B.pack $ fmap (fromIntegral . ord) s) 

texttype :: TextType -> Word8
texttype GENERIC_TEXT         = 0x01
texttype COPYRIGHT_NOTICE     = 0x02
texttype SEQUENCE_NAME        = 0x03
texttype INSTRUMENT_NAME      = 0x04
texttype LYRICS               = 0x05
texttype MARKER               = 0x06
texttype CUE_POINT            = 0x07

