{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.WriteFile
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Write a MIDI file.
--
--------------------------------------------------------------------------------


module ZMidi.Core.WriteFile 
  (
  -- * Write a Midi structure to file
    writeMidi
  ) where

import ZMidi.Core.Datatypes


import Data.Binary.Put                  -- package: binary

import Control.Applicative
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Int
import Data.Word

import System.IO


writeMidi :: FilePath -> MidiFile -> IO ()
writeMidi filename midi = 
    openBinaryFile filename WriteMode        >>= \hdl -> 
    L.hPut hdl (runPut $ putMidiFile midi)   >>
    hClose hdl                    

putMidiFile :: MidiFile -> PutM ()
putMidiFile (MidiFile hdr trks) = 
    putHeader hdr *> mapM_ putTrack trks
  
putHeader :: Header -> PutM ()
putHeader (Header fmt n td) =
    outString "MThd"  *>  putWord32be 6 *> 
    putHFormat fmt    *>  putWord16be n *>  putTimeDivision td


putTrack :: Track -> PutM ()
putTrack (Track ms) = 
    outString "MTrk" *> (putWord32be $ fromIntegral $ L.length bs)
                     *> putLazyByteString bs
  where 
    bs = runPut (mapM_ putMessage ms) 


putHFormat :: HFormat -> PutM ()
putHFormat MF0 = putWord16be 0
putHFormat MF1 = putWord16be 1
putHFormat MF2 = putWord16be 2

putTimeDivision :: TimeDivision -> PutM ()
putTimeDivision (FPS n) = putWord16be (n `setBit` 15)
putTimeDivision (TPB n) = putWord16be (n `clearBit` 15)



putMessage :: Message -> PutM () 
putMessage (dt,evt) = varlen dt *> putEvent evt

putEvent :: Event -> PutM ()
putEvent (VoiceEvent  evt) = putVoiceEvent evt
putEvent (SystemEvent evt) = putSystemEvent evt
putEvent (MetaEvent   evt) = putMetaEvent evt
  
    
putVoiceEvent :: VoiceEvent -> PutM ()
putVoiceEvent (NoteOff c n v)         = out3 (0x8 `u4l4` c) n v 
putVoiceEvent (NoteOn c n v)          = out3 (0x9 `u4l4` c) n v 
putVoiceEvent (NoteAftertouch c n v)  = out3 (0xA `u4l4` c) n v
putVoiceEvent (Controller c n v)      = out3 (0xB `u4l4` c) n v
putVoiceEvent (ProgramChange c n)     = out2 (0xC `u4l4` c) n
putVoiceEvent (ChanAftertouch c v)    = out2 (0xD `u4l4` c) v  
putVoiceEvent (PitchBend c v)         = out1 (0xE `u4l4` c) *> putWord16be v


putMetaEvent :: MetaEvent -> PutM ()
putMetaEvent (TextEvent ty ss)                = 
    out2 0xFF (texttype ty) *> (varlen $ fromIntegral $ length ss) *> outString ss
  
putMetaEvent (SequenceNumber n)               = out3 0xFF 0x00 2 *> putWord16be n
  
putMetaEvent (ChannelPrefix i ch)             = out3 0xFF 0x20 i *> out1 ch
  
putMetaEvent (EndOfTrack)                     = out3 0xFF 0x2F 0
  
putMetaEvent (SetTempo t)                     = out3 0xFF 0x51 3 *> putWord24be t
  
putMetaEvent (SMPTEOffset hr mn sc fr sfr)    =
    out3 0xFF 0x54 5 *> out5 hr mn sc fr sfr
  
putMetaEvent (TimeSignature nmr dnm met nps)  =
    out3 0xFF 0x58 4 *> out4 nmr dnm met nps    
  
putMetaEvent (KeySignature ky sc)             =
    out3 0xFF 0x59 2 *> out2 (wrapint ky) (wscale sc)

putMetaEvent (SSME i ws)                      =  
    out2 0xFF 0x7F *> varlen i *> outBytes ws


-- WARNING - what is DataEvent???????
putSystemEvent :: SystemEvent -> PutM ()
putSystemEvent (SysEx i ws)   = out1 0xF0 *> varlen i *> outBytes ws
putSystemEvent (DataEvent w8) = putWord8 w8 
  
    
  



--------------------------------------------------------------------------------
-- Output helpers

infixr 5 `u4l4`

u4l4 :: Word8 -> Word8 -> Word8
a `u4l4` b = (a `shiftL` 4) + b 

out1            :: Word8 -> PutM () 
out1            = putWord8

out2            :: Word8 -> Word8 -> PutM () 
out2 a b        = putWord8 a *> putWord8 b

out3            :: Word8 -> Word8 -> Word8 -> PutM ()
out3 a b c      = putWord8 a *> putWord8 b *> putWord8 c

out4            :: Word8 -> Word8 -> Word8 -> Word8 -> PutM () 
out4 a b c d    = putWord8 a *> putWord8 b *> putWord8 c *> putWord8 d

out5            :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> PutM ()
out5 a b c d e  = putWord8 a *> putWord8 b *> putWord8 c
                             *> putWord8 d *> putWord8 e

   

wrapint :: Int8 -> Word8
wrapint i | i < 0     = fromIntegral $ i' + 256
          | otherwise = fromIntegral i
  where
    i' :: Int
    i' = fromIntegral i  
    
wscale :: ScaleType -> Word8
wscale MAJOR = 0x00
wscale MINOR = 0x01


putWord24be :: Word32 -> PutM ()
putWord24be i = out3 c b a 
  where 
  (a, r1)   = lowerEight i     
  (b, r2)   = lowerEight r1
  (c, _)    = lowerEight r2
  
    

lowerEight :: (Bits a, Integral a) => a -> (Word8, a)    
lowerEight n = (fromIntegral lower8, remain)
  where
    remain = n `shiftR` 8
    lower8 = n .&. 0xff 
      
varlen :: Word32 -> PutM ()
varlen = step . toVarlen where
    step (V1 a)          = out1 a
    step (V2 a b)        = out2 a b
    step (V3 a b c)      = out3 a b c
    step (V4 a b c d)    = out4 a b c d
    

outString :: String -> PutM ()    
outString s = putLazyByteString (L.pack $ fmap (fromIntegral . ord) s) 

{-
outChars :: String -> MidiOut
outChars []     = id
outChars (s:ss) = out1 (fromIntegral $ ord s) . outChars ss
-}

outBytes :: [Word8] -> PutM () 
outBytes []     = return ()
outBytes (s:ss) = putWord8 s *> outBytes ss


texttype :: TextType -> Word8
texttype GENERIC_TEXT         = 0x01
texttype COPYRIGHT_NOTICE     = 0x02
texttype SEQUENCE_NAME        = 0x03
texttype INSTRUMENT_NAME      = 0x04
texttype LYRICS               = 0x05
texttype MARKER               = 0x06
texttype CUE_POINT            = 0x07

