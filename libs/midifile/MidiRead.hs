

module MidiRead (readMidi) where

import MidiDatatypes

import Control.Monad
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Int
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as L


readMidi :: FilePath -> IO MidiFile
readMidi path = do
  bs <- L.readFile path
  let ans = runGet getMidiFile bs
  return ans                  
                    
                    
getMidiFile :: Get MidiFile  
getMidiFile = do
  hdr@(Header _ n _) <- getHeader
  mts <- replicateM (fromIntegral n) getTrack
  return $ MidiFile hdr (catMaybes mts)

getHeader :: Get Header  
getHeader = do
  assertChars "MThd"
  i <- getWord32be
  when (i /= 6) (error $ "Header failed " ++ show i ++ " (not 6)")
  ty <- getFormat
  nm <- getWord16be
  td <- getTimeDivision
  return $ Header ty nm td
  
  

getFormat :: Get HFormat
getFormat = do
  fmt <- getWord16be
  case fmt of
    0 -> return MF0
    1 -> return MF1
    2 -> return MF2
    _ -> error $ "Unrecognized format " ++ show fmt




getTimeDivision :: Get TimeDivision
getTimeDivision = do
  i <- getWord16be
  case (i `testBit` 15) of
    True -> return $ FPS (i `clearBit` 15)
    False -> return $ TPB i
    
getTrack :: Get (Maybe Track)
getTrack = assertChars "MTrk" >>= \a -> f a
  where f False = return Nothing 
        f True  = do i <- getWord32be
                     bs <- getLazyByteString (fromIntegral i)
                     let evts = runGet (getEvts) bs
                     return $ Just (Track evts)  
             
getEvts :: Get [Event]
getEvts = recGetEvts []
  where
    recGetEvts acc = do
      end <- isEmpty
      case end of -- x == fromIntegral l of
        True -> return (reverse acc)
        False -> do { e <- getEvent ; recGetEvts (e:acc) }
                   
                     
getEvent :: Get Event 
getEvent = do
  dt <- getVarlen
  ty <- getEventType
  return $ Event dt ty
  
  
  
getEventType :: Get EventType
getEventType = getWord8 >>= \i -> f i
  where f 0xFF = do { j <- getWord8; getMetaEvent j }
        f i    = let typ = (i .&. 0xF0) `shiftR` 4; chan = i .&. 0x0F;
                 in chanEvt typ chan

chanEvt :: Word8 -> Word8 -> Get EventType
chanEvt 0x8 ch = eventWithTwoParams NoteOff ch  
chanEvt 0x9 ch = eventWithTwoParams NoteOn ch
chanEvt 0xA ch = eventWithTwoParams NoteAftertouch ch
chanEvt 0xB ch = eventWithTwoParams Controller ch
chanEvt 0xC ch = eventWithOneParam  ProgramChange ch
chanEvt 0xD ch = eventWithOneParam  ChanAftertouch ch
chanEvt 0xE ch = do
  a <- getWord8 
  b <- getWord8
  let lsb = fromIntegral (clearBit a 7)
  let msb = fromIntegral (clearBit b 7)
  return $ PitchBend ch ((msb `shiftL` 7) + lsb)
  
chanEvt evt _   = error $ "chanEvt match on " ++ show evt                           

eventWithOneParam :: (Word8 -> Word8 -> EventType) -> Word8 -> Get EventType                          
eventWithOneParam cnstr ch = do
  a <- getWord8
  return $ cnstr ch a

eventWithTwoParams :: (Word8 -> Word8 -> Word8 -> EventType) 
                   -> Word8 -> Get EventType   
eventWithTwoParams cnstr ch = do
  a <- getWord8
  b <- getWord8
  return $ cnstr ch a b
  
  

getMetaEvent :: Word8 -> Get EventType
getMetaEvent 0x01 = getTextEvt GENERIC_TEXT
getMetaEvent 0x02 = getTextEvt COPYWRIGHT_NOTICE
getMetaEvent 0x03 = getTextEvt SEQUENCE_NAME
getMetaEvent 0x04 = getTextEvt INSTRUMENT_NAME
getMetaEvent 0x05 = getTextEvt LYRICS
getMetaEvent 0x06 = getTextEvt MARKER
getMetaEvent 0x07 = getTextEvt CUE_POINT
    
getMetaEvent 0x2F = do 
  assertWord8 0
  return EndOfTrack
  
getMetaEvent 0x51 = do 
  assertWord8 3
  i <- getWord24be
  return (SetTempo i) 
  
getMetaEvent 0x54 = do 
  assertWord8 5
  hr <- getWord8
  mn <- getWord8
  sc <- getWord8
  fr <- getWord8
  sf <- getWord8
  return $ SMPTEOffset hr mn sc fr sf
  
getMetaEvent 0x58 = do 
  assertWord8 4
  nr <- getWord8 
  dm <- getWord8
  mo <- getWord8
  nd <- getWord8
  return (TimeSignature nr dm mo nd) 

getMetaEvent 0x59 = do 
  assertWord8 2
  ky <- getWord8
  sc <- getWord8
  return (KeySignature (unwrapint ky) (undscale sc)) 
  
getMetaEvent i = do
  error $ "unregonized match in getMetaEvent " ++ show i


getTextEvt :: TextType -> Get EventType  
getTextEvt ty = do
  len <- getVarlen
  bs <- getLazyByteString (fromIntegral len)
  return $ TextEvent ty (L.unpack bs)
  
  
--------------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------------

getWord24be :: Get Word32
getWord24be = do
  hi8 <- getWord8
  lo16 <- getWord16be
  return $ ((fromIntegral hi8) `shiftL` 8) + (fromIntegral lo16)
  
getVarlen :: Get Word32
getVarlen = recVarlen 0
  where
    recVarlen acc = do
      i <- getWord8
      case (i `testBit` 7) of
        True -> recVarlen ((acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F))
        False -> return ((acc `shiftL` 7) + (fromIntegral i))
        
          
unwrapint :: Word8 -> Int8
unwrapint i 
  | i > 128   = (fromIntegral i) - 256
  | otherwise = fromIntegral i
  
undscale :: Word8 -> Scale
undscale 0 = MAJOR
undscale 1 = MINOR
undscale i = error $ "undscale " ++ show i

-- this does nothing useful (yet), really we need position tracking
assertWord8 :: Word8 -> Get (Maybe Word8)
assertWord8 i = getWord8 >>= \j -> f j
  where f j | i == j    = return $ Just i
            | otherwise = return Nothing 


assertChars :: String -> Get Bool
assertChars s = let i = fromIntegral $ length s in do
  bs <- getLazyByteString i
  return (s == L.unpack bs)
                  
    