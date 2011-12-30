{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.WriteFile
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
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
import ZMidi.Core.Internal.ExtraTypes

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
  
putHeader :: MidiHeader -> PutM ()
putHeader (MidiHeader fmt n td) =
    putString "MThd"  *>  putWord32be 6 *> 
    putFormat fmt     *>  putWord16be n *>  putTimeDivision td


putTrack :: MidiTrack -> PutM ()
putTrack (MidiTrack ms) = 
    putString "MTrk" *> (putWord32be $ fromIntegral $ L.length bs)
                     *> putLazyByteString bs
  where 
    bs = runPut (mapM_ putMessage ms) 


putFormat :: MidiFormat -> PutM ()
putFormat MF0 = putWord16be 0
putFormat MF1 = putWord16be 1
putFormat MF2 = putWord16be 2

putTimeDivision :: MidiTimeDivision -> PutM ()
putTimeDivision (FPS n) = putWord16be (n `setBit`   15)
putTimeDivision (TPB n) = putWord16be (n `clearBit` 15)



putMessage :: MidiMessage -> PutM () 
putMessage (dt,evt) = putVarlen (fromIntegral dt) *> putEvent evt

putEvent :: MidiEvent -> PutM ()
putEvent (DataEvent e)        = putDataEvent  e
putEvent (VoiceEvent e)       = putVoiceEvent e
putEvent (SysExEvent e)       = putSysExEvent e
putEvent (SysCommonEvent e)   = putSysCommonEvent e
putEvent (SysRealTimeEvent e) = putSysRealTimeEvent e
putEvent (MetaEvent e)        = putMetaEvent  e
  

putDataEvent :: MidiDataEvent -> PutM ()
putDataEvent (MidiDataEvent tag) = putWord8 tag
    
putVoiceEvent :: MidiVoiceEvent -> PutM ()
putVoiceEvent (NoteOff c n v)         = 
    putWord8 (0x8 `u4l4` c) *> putWord8 n *> putWord8 v 

putVoiceEvent (NoteOn c n v)          = 
    putWord8 (0x9 `u4l4` c) *> putWord8 n *> putWord8 v 

putVoiceEvent (NoteAftertouch c n v)  = 
    putWord8 (0xA `u4l4` c) *> putWord8 n *> putWord8 v

putVoiceEvent (Controller c n v)      = 
    putWord8 (0xB `u4l4` c) *> putWord8 n *> putWord8 v

putVoiceEvent (ProgramChange c n)     = 
    putWord8 (0xC `u4l4` c) *> putWord8 n

putVoiceEvent (ChanAftertouch c v)    = 
    putWord8 (0xD `u4l4` c) *> putWord8 v  

putVoiceEvent (PitchBend c v)         = 
    putWord8 (0xE `u4l4` c) *> putWord16be v


putSysExEvent :: MidiSysExEvent -> PutM ()
putSysExEvent (SysEx n ws) = 
    putWord8 0xF0 *> putVarlen n *> mapM_ putWord8 ws


putSysCommonEvent :: MidiSysCommonEvent -> PutM ()
putSysCommonEvent (QuarterFrame sb)        = 
    putWord8 0xF1 *> putWord8 sb

putSysCommonEvent (SongPosPointer lsb msb) = 
    putWord8 0xF2 *> putWord8 lsb *> putWord8 msb

putSysCommonEvent (SongSelect w)           = 
    putWord8 0xF3 *> putWord8 w

putSysCommonEvent (Common_undefined tag)   = 
    putWord8 tag

putSysCommonEvent TuneRequest              = 
    putWord8 0xF6

putSysCommonEvent EOX                      = 
    putWord8 0xF7


putSysRealTimeEvent :: MidiSysRealTimeEvent -> PutM ()
putSysRealTimeEvent TimingClock            = putWord8 0xF8
putSysRealTimeEvent (RT_undefined tag)     = putWord8 tag
putSysRealTimeEvent StartSequence          = putWord8 0xFA
putSysRealTimeEvent ContinueSequence       = putWord8 0xFB
putSysRealTimeEvent StopSequence           = putWord8 0xFC
putSysRealTimeEvent ActiveSensing          = putWord8 0xFE
putSysRealTimeEvent SystemReset            = putWord8 0xFF


putMetaEvent :: MidiMetaEvent -> PutM ()
putMetaEvent (TextEvent ty ss)                = 
    putWord8 0xFF *> putWord8 (texttype ty) 
                  *> putVarlen   (fromIntegral $ length ss) 
                  *> putString ss
  
putMetaEvent (SequenceNumber n)               = 
    putWord8 0xFF *> putWord8 0x00 *> prefixLen 2 (putWord16be n)
  
putMetaEvent (ChannelPrefix i ch)             = 
    putWord8 0xFF *> putWord8 0x20 *> prefixLen i (putWord8 ch)
  
putMetaEvent (EndOfTrack)                     = 
    putWord8 0xFF *> putWord8 0x2F *> prefixLen 0 (pure ())
  
putMetaEvent (SetTempo t)                     = 
    putWord8 0xFF *> putWord8 0x51 *> prefixLen 3 (putWord24be t)
  
putMetaEvent (SMPTEOffset hr mn sc fr sfr)    =
    putWord8 0xFF *> putWord8 0x54 *> prefixLen 5 body
 where
    body = putWord8 hr *> putWord8 mn *> putWord8 sc 
                       *> putWord8 fr *> putWord8 sfr
  
putMetaEvent (TimeSignature nmr dnm met nps)  =
    putWord8 0xFF *> putWord8 0x58 *> prefixLen 4 body
  where
    body = putWord8 nmr *> putWord8 dnm *> putWord8 met *> putWord8 nps    
  
putMetaEvent (KeySignature ky sc)             =
    putWord8 0xFF *> putWord8 0x59 *> prefixLen 2 body
  where
    body = putWord8 (wrapint ky) *> putWord8 (wscale sc)

putMetaEvent (SSME i ws)                      =  
    putWord8 0xFF *> putWord8 0x7F *> putVarlen i *> mapM_ putWord8 ws


    
  



--------------------------------------------------------------------------------
-- Output helpers

prefixLen :: Word8 -> PutM () -> PutM ()
prefixLen n out = putWord8 n *> out 


infixr 5 `u4l4`

u4l4 :: Word8 -> Word8 -> Word8
a `u4l4` b = (a `shiftL` 4) + b 
  

wrapint :: Int8 -> Word8
wrapint i | i < 0     = fromIntegral $ i' + 256
          | otherwise = fromIntegral i
  where
    i' :: Int
    i' = fromIntegral i  
    
wscale :: MidiScaleType -> Word8
wscale MAJOR = 0x00
wscale MINOR = 0x01


putWord24be :: Word32 -> PutM ()
putWord24be i = putWord8 c *> putWord8 b *> putWord8 a 
  where 
  (a, r1)   = lowerEight i     
  (b, r2)   = lowerEight r1
  (c, _)    = lowerEight r2
  
    

lowerEight :: (Bits a, Integral a) => a -> (Word8, a)    
lowerEight n = (fromIntegral lower8, remain)
  where
    remain = n `shiftR` 8
    lower8 = n .&. 0xff 
      
putVarlen :: Word32 -> PutM ()
putVarlen = step . toVarlen where
    step (V1 a)          = putWord8 a
    step (V2 a b)        = putWord8 a *> putWord8 b
    step (V3 a b c)      = putWord8 a *> putWord8 b *> putWord8 c
    step (V4 a b c d)    = putWord8 a *> putWord8 b *> putWord8 c *> putWord8 d
    

putString :: String -> PutM ()    
putString s = putLazyByteString (L.pack $ fmap (fromIntegral . ord) s) 




texttype :: MidiTextType -> Word8
texttype GENERIC_TEXT         = 0x01
texttype COPYRIGHT_NOTICE     = 0x02
texttype SEQUENCE_NAME        = 0x03
texttype INSTRUMENT_NAME      = 0x04
texttype LYRICS               = 0x05
texttype MARKER               = 0x06
texttype CUE_POINT            = 0x07

