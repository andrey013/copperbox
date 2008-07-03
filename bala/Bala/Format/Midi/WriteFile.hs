
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Midi.WriteFile
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Write a MIDI file using Binary.Put.
--
--------------------------------------------------------------------------------


module Bala.Format.Midi.WriteFile (
    -- * Write a Midi structure to file
    writeMidi
  ) where

import Bala.Format.Midi.Datatypes


import Data.Bits
import Data.Word
import Data.Int
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as SBS

writeMidi :: FilePath -> MidiFile -> IO ()
writeMidi path mf = do 
  let lbs = runPut (putMidiFile mf)
  L.writeFile path lbs     
                    

putMidiFile :: MidiFile -> Put
putMidiFile (MidiFile hdr trks) = do
  putHeader hdr
  mapM_ putTrack trks
  flush
  
putHeader :: Header -> Put
putHeader (Header fmt n td) = do
  putString     "MThd"
  putWord32be   6
  putHFormat    fmt
  putWord16be   n
  putTimeDivision td


putTrack :: Track -> Put
putTrack (Track es) = do
  putString     "MTrk"
  let contents = runPut (mapM_ putMessage es)
  let len = L.length contents
  putWord32be (fromIntegral len)
  putLazyByteString contents
  flush


putHFormat :: HFormat -> Put
putHFormat MF0 = putWord16be 0
putHFormat MF1 = putWord16be 1
putHFormat MF2 = putWord16be 2

putTimeDivision :: TimeDivision -> Put
putTimeDivision (FPS n) = putWord16be (n `setBit` 15)
putTimeDivision (TPB n) = putWord16be (n `clearBit` 15)


putTextType :: TextType -> Put
putTextType GENERIC_TEXT       = putWord8 1
putTextType COPYRIGHT_NOTICE  = putWord8 2 
putTextType SEQUENCE_NAME      = putWord8 3
putTextType INSTRUMENT_NAME    = putWord8 4
putTextType LYRICS             = putWord8 5
putTextType MARKER             = putWord8 6
putTextType CUE_POINT          = putWord8 7

putMessage :: Message -> Put
putMessage (dt,evt) = do
  putVarlen dt
  putEvent  evt

putEvent :: Event -> Put
putEvent (VoiceEvent  evt) = putVoiceEvent evt
putEvent (SystemEvent evt) = putSystemEvent evt
putEvent (MetaEvent   evt) = putMetaEvent evt
  
    
putVoiceEvent :: VoiceEvent -> Put 
putVoiceEvent (NoteOff ch nt vel) = do
  putWord8 (0x8 `chanShift` ch)
  putWord8 nt
  putWord8 vel 
  
putVoiceEvent (NoteOn ch nt vel) = do
  putWord8 (0x9 `chanShift` ch)
  putWord8 nt
  putWord8 vel 
  
putVoiceEvent (NoteAftertouch ch nt val) = do
  putWord8 (0xA `chanShift` ch)
  putWord8 nt
  putWord8 val
  
putVoiceEvent (Controller ch nt val) = do
  putWord8 (0xB `chanShift` ch)
  putWord8 nt
  putWord8 val
  
putVoiceEvent (ProgramChange ch num) = do
  putWord8 (0xC `chanShift` ch)
  putWord8 num
  
putVoiceEvent (ChanAftertouch ch val) = do
  putWord8 (0xD `chanShift` ch)
  putWord8 val
  
putVoiceEvent (PitchBend ch val) = do
  putWord8 ( (0xE::Word8) `chanShift` ch)
  putWord16be val              -- 2DO


putMetaEvent :: MetaEvent -> Put
putMetaEvent (TextEvent ty cs) = do
  putWord8      0xFF
  putTextType   ty
  putVarlen     (length cs)
  putByteString (SBS.pack cs)
  
putMetaEvent (SequenceNumber n) = do
  putWord8      0xFF
  putWord8      0
  putWord8      2
  putWord16be   n
  
putMetaEvent (ChannelPrefix ch) = do
  putWord8 0xFF
  putWord8 0x20
  putWord8 1
  putWord8 ch
  
putMetaEvent (EndOfTrack) = do
  putWord8 0xFF
  putWord8 0x2F
  putWord8 0
  
putMetaEvent (SetTempo t) = do
  putWord8 0xFF
  putWord8 0x51
  putWord8 3           
  putWord24be t
  
putMetaEvent (SMPTEOffset hr mn sc fr sfr) = do
  putWord8 0xFF
  putWord8 0x54
  putWord8 5
  putWord8 hr
  putWord8 mn
  putWord8 sc
  putWord8 fr
  putWord8 sfr
  
putMetaEvent (TimeSignature nmr dnm met nps) = do
  putWord8 0xFF
  putWord8 0x58   
  putWord8 4
  putWord8 nmr
  putWord8 dnm
  putWord8 met
  putWord8 nps    
  
putMetaEvent (KeySignature ky sc) = do
  putWord8 0xFF
  putWord8 0x59
  putWord8 2 
  putWord8 (wrapint ky)
  putScale sc

putMetaEvent (SSME i ws) = do
  putVarlen i
  putBytes ws

putSystemEvent :: SystemEvent -> Put    
putSystemEvent (SysEx i ws) = do
  putVarlen i
  putBytes ws
  
    
putScale :: ScaleType -> Put
putScale MAJOR = putWord8 0
putScale MINOR = putWord8 1
  
putVarlen :: Integral a => a -> Put
putVarlen i = mapM_ putWord8 (varlen i)



--------------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------------

wrapint :: Int8 -> Word8
wrapint i
  | i < 0     = fromIntegral $ i + 256
  | otherwise = fromIntegral i

putBytes :: [Word8] -> Put
putBytes xs = mapM_ putWord8 xs


putString :: String -> Put
putString = putByteString . SBS.pack
  
putWord24be :: (Integral n, Bits n) => n -> Put
putWord24be i = do
  putWord8 c
  putWord8 b
  putWord8 a
  where 
  (r, a)    = split i     
  (r', b)   = split r
  (_, c)    = split r'
    
  split n = (remain, lower8)
    where
      remain = n `shiftR` 8
      lower8 :: Word8
      lower8 = fromIntegral $ n .&. 0xff   


varlen :: Integral a => a -> [Word8]
varlen a = varlenSplit $ fromIntegral a

      
infixr 5 `chanShift`


-- to do ... a better version ...
chanShift :: Word8 -> Word8 -> Word8
a `chanShift` b = val'
  where val =  ((toInteger a) `shiftL` 4) +  toInteger b
        val' = case val < 256 of
                  True -> fromIntegral val
                  False -> error $ "chanShift applied to values that produce " ++ show val
              
               