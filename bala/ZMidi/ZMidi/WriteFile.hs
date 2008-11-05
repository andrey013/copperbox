
--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.WriteFile
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Write a MIDI file.
--
--------------------------------------------------------------------------------


module ZMidi.WriteFile (
    -- * Write a Midi structure to file
    writeMidi
  ) where

import ZMidi.Datatypes

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char (ord)
import qualified Data.Foldable as F
import Data.Int
import qualified Data.Sequence as S
import Data.Word

import System.IO


type MidiBlock = B.ByteString

type MidiOut = MidiBlock -> MidiBlock

writeMidi :: FilePath -> MidiFile -> IO ()
writeMidi path midi = let midistream = putMidiFile midi $ B.empty in do
    h <- openBinaryFile path WriteMode
    B.hPut h midistream
    hClose h    
                    

putMidiFile :: MidiFile -> MidiOut
putMidiFile (MidiFile hdr trks) = putHeader hdr . fns
  where fns = F.foldr (\a f -> putTrack a . f) id trks
  
putHeader :: Header -> MidiOut
putHeader (Header fmt n td) =
    outChars "MThd" . outW32 6 . putHFormat fmt . outW16 n . putTimeDivision td


putTrack :: Track -> MidiOut
putTrack (Track se) = 
    let fn = F.foldr (\a f -> putMessage a . f) id se
        bs = fn B.empty
    in outChars "MTrk" . (outW32 $ fromIntegral $ B.length bs) . (B.append bs) 


putHFormat :: HFormat -> MidiOut
putHFormat MF0 = outW16 0
putHFormat MF1 = outW16 1
putHFormat MF2 = outW16 2

putTimeDivision :: TimeDivision -> MidiOut
putTimeDivision (FPS n) = outW16 (n `setBit` 15)
putTimeDivision (TPB n) = outW16 (n `clearBit` 15)



putMessage :: Message -> MidiOut
putMessage (Message (dt,evt)) = varlen dt . putEvent evt

putEvent :: Event -> MidiOut
putEvent (VoiceEvent  evt) = putVoiceEvent evt
putEvent (SystemEvent evt) = putSystemEvent evt
putEvent (MetaEvent   evt) = putMetaEvent evt
  
    
putVoiceEvent :: VoiceEvent -> MidiOut 
putVoiceEvent (NoteOff c n v)         = out3 (0x8 `u4l4` c) n v 
putVoiceEvent (NoteOn c n v)          = out3 (0x9 `u4l4` c) n v 
putVoiceEvent (NoteAftertouch c n v)  = out3 (0xA `u4l4` c) n v
putVoiceEvent (Controller c n v)      = out3 (0xB `u4l4` c) n v
putVoiceEvent (ProgramChange c n)     = out2 (0xC `u4l4` c) n
putVoiceEvent (ChanAftertouch c v)    = out2 (0xD `u4l4` c) v  
putVoiceEvent (PitchBend c v)         = out1 (0xE `u4l4` c) . outW16 v


putMetaEvent :: MetaEvent -> MidiOut
putMetaEvent (TextEvent ty ss)                = 
    out2 0xFF (texttype ty) . (varlen $ fromIntegral $ length ss) . outString ss
  
putMetaEvent (SequenceNumber n)               = out3 0xFF 0 2 . outW16 n
  
putMetaEvent (ChannelPrefix ch)               = out3 0xFF 0x20 1 . out1 ch
  
putMetaEvent (EndOfTrack)                     = out3 0xFF 0x2F 0
  
putMetaEvent (SetTempo t)                     = out3 0xFF 0x51 3 . outW24 t
  
putMetaEvent (SMPTEOffset hr mn sc fr sfr)    =
    out3 0xFF 0x54 5 . out5 hr mn sc fr sfr
  
putMetaEvent (TimeSignature nmr dnm met nps)  =
    out3 0xFF 0x58 4 . out4 nmr dnm met nps    
  
putMetaEvent (KeySignature ky sc)             =
    out3 0xFF 0x59 2 . out2 (wrapint ky) (wscale sc)

putMetaEvent (SSME i ws)                      =  varlen i . outBytes ws

putSystemEvent :: SystemEvent -> MidiOut    
putSystemEvent (SysEx i ws) = varlen i . outBytes ws
  
    
  



--------------------------------------------------------------------------------
-- Output helpers

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

outChars :: String -> MidiOut
outChars []     = id
outChars (s:ss) = out1 (fromIntegral $ ord s) . outChars ss

outBytes :: [Word8] -> MidiOut
outBytes []     = id
outBytes (s:ss) = out1 s . outBytes ss

texttype :: TextType -> Word8
texttype GENERIC_TEXT         = 0x01
texttype COPYRIGHT_NOTICE     = 0x02
texttype SEQUENCE_NAME        = 0x03
texttype INSTRUMENT_NAME      = 0x04
texttype LYRICS               = 0x05
texttype MARKER               = 0x06
texttype CUE_POINT            = 0x07

