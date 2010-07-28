{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.ReadFile
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A MIDI file parser. 
--
--------------------------------------------------------------------------------

module ZMidi.Core.ReadFile 
  (
  -- * Read a MIDI file
  -- $readmididoc
    readMidi
  
  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.ParserMonad


import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy   as L
import Data.Word
import Numeric


readMidi :: FilePath -> IO (Either ParseErr MidiFile)
readMidi filename =
    liftM (runParser `flip` midiFile) (L.readFile filename)

--------------------------------------------------------------------------------
-- 

    
midiFile :: ParserM MidiFile  
midiFile = {- printHexAll >> -} do
    hdr   <- header
    let i  = trackCount hdr
    trks  <- count i track
    return $ MidiFile hdr trks   
  where
    trackCount :: Header -> Int 
    trackCount (Header _ n _) = fromIntegral n

header :: ParserM Header  
header = Header <$> (assertString "MThd" *> assertWord32 (6::Int) *> format)
                <*> word16be
                <*> timeDivision



track :: ParserM Track
track = liftM Track (trackHeader >>= getMessages)

trackHeader :: ParserM Word32
trackHeader = assertString "MTrk" >> word32be


-- wrong... 
getMessages :: Word32 -> ParserM [Message]
getMessages i = boundRepeat (fromIntegral i) message


message :: ParserM Message
message = (,) <$>  deltaTime <*> (uncurry next =<< word8split)
  where  
    next code chan  
        | code == 0xF && chan == 0xF  = MetaEvent   <$> (word8 >>= metaEvent)         
        | code == 0xF                 = SystemEvent <$> systemEvent
        | code >= 0x8 && code <  0xF  = VoiceEvent  <$> voiceEvent code chan
        | otherwise                   = reportError $ unwords 
                                          [ "unrecognized message "
                                          , hexStr ((code `shiftL` 4) + chan)
                                          , "code:"
                                          , hex2 code
                                          , "chan:"
                                          , hex2 chan
                                          ]


hex2 :: Integral a => a -> String
hex2 = ($ "") . showHex 

deltaTime :: ParserM Word32
deltaTime = getVarlen
   
voiceEvent :: Word8 -> Word8 -> ParserM VoiceEvent
voiceEvent 0x8 ch = (NoteOff ch)        <$> word8 <*> word8
voiceEvent 0x9 ch = (NoteOn ch)         <$> word8 <*> word8
voiceEvent 0xA ch = (NoteAftertouch ch) <$> word8 <*> word8
voiceEvent 0xB ch = (Controller ch)     <$> word8 <*> word8
voiceEvent 0xC ch = (ProgramChange ch)  <$> word8 
voiceEvent 0xD ch = (ChanAftertouch ch) <$> word8 
voiceEvent 0xE ch = (PitchBend ch)      <$> word16be
voiceEvent z   _  = reportError $ "voiceEvent " ++ hexStr z 




metaEvent :: Word8 -> ParserM MetaEvent
metaEvent 0x00 = SequenceNumber   <$> (assertWord8 2     *> word16be)
metaEvent 0x01 = textEvent GENERIC_TEXT 
metaEvent 0x02 = textEvent COPYRIGHT_NOTICE
metaEvent 0x03 = textEvent SEQUENCE_NAME
metaEvent 0x04 = textEvent INSTRUMENT_NAME
metaEvent 0x05 = textEvent LYRICS
metaEvent 0x06 = textEvent MARKER
metaEvent 0x07 = textEvent CUE_POINT
metaEvent 0x20 = ChannelPrefix    <$> word8             <*> word8
metaEvent 0x2F = EndOfTrack       <$   assertWord8 0  
metaEvent 0x51 = SetTempo         <$> (assertWord8 3     *> word24be)
metaEvent 0x54 = SMPTEOffset      <$> (assertWord8 5     *> word8) 
                                  <*> word8             <*> word8
                                  <*> word8             <*> word8
metaEvent 0x58 = TimeSignature    <$> (assertWord8 4     *> word8)
                                  <*> word8             <*> word8 
                                  <*> word8 
metaEvent 0x59 = KeySignature     <$> (assertWord8 2     *> int8) 
                                  <*> scale
metaEvent 0x7F = (uncurry SSME)   <$> getVarlenBytes     
metaEvent z    = reportError $ "unreconized meta-event " ++ hexStr z


systemEvent :: ParserM SystemEvent
systemEvent = (uncurry SysEx) <$> getVarlenBytes
                      
                          
format :: ParserM HFormat
format = word16be >>= fn 
  where 
    fn 0 = return MF0
    fn 1 = return MF1
    fn 2 = return MF2
    fn z = reportError $ 
              "getFormat - unrecognized file format " ++ hexStr z
        
timeDivision :: ParserM TimeDivision
timeDivision = division <$> word16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i


scale :: ParserM ScaleType
scale = word8 >>= fn 
  where
    fn 0 = return MAJOR
    fn 1 = return MINOR
    fn z = reportError $ "scale expecting 0 or 1, got " ++ hexStr z
    
    
textEvent :: TextType -> ParserM MetaEvent
textEvent ty = (TextEvent ty . snd) <$> getVarlenText

--------------------------------------------------------------------------------
-- helpers

{-
-- enumerate doesn't really seem worth it for scale etc... 
enumerate :: Integral a => [ans] -> a -> String -> ParserM ans
enumerate []     _ msg = reportError msg
enumerate (x:xs) i msg | i == 0    = return x
                       | otherwise = enumerate xs (i-1) msg

-}


word8split :: ParserM (Word8,Word8) 
word8split = split <$> word8 
  where
    split i = ((i .&. 0xF0) `shiftR` 4, i .&. 0x0F)

 
assertWord8 :: Word8 -> ParserM Word8
assertWord8 i = postCheck word8 (==i) msg
  where 
    msg = "assertWord8 - input did not match " ++ show i
             
assertWord32 :: Integral a => a -> ParserM Word32
assertWord32 i = postCheck word32be ((==i) . fromIntegral) msg
  where
    msg = "assertWord32 - input did not match " ++ show i

assertString :: String -> ParserM String
assertString s = postCheck (text $ length s) (==s) msg
  where
    msg = "assertString - input did not match " ++ s


getVarlenText :: ParserM (Word32,String)  
getVarlenText = gencount getVarlen char8

getVarlenBytes :: ParserM (Word32,[Word8]) 
getVarlenBytes = gencount getVarlen word8


getVarlen :: ParserM Word32
getVarlen = buildWhile (`testBit` 7) merge merge 0 word8
  where
    merge i acc = (acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F)
   



-- | Build a value by while the test holds. When the test fails 
-- the position is not backtracked, instead we use the \"failing\"
-- element with @lastOp@ potentially still building the value 
-- with it.
-- 
buildWhile :: (a -> Bool) 
           -> (a -> b -> b) 
           -> (a -> b -> b) 
           -> b 
           -> ParserM a 
           -> ParserM b
buildWhile test op lastOp initial p = step where
    step = p >>= \ans -> 
      if test ans then (step >>= \acc -> return $ ans `op` acc)
                  else (return $ ans `lastOp` initial)


-- | Apply parse then apply the check, if the check fails report
-- the error message. 
postCheck :: ParserM a -> (a -> Bool) -> String -> ParserM a
postCheck p check msg = p >>= \ans -> 
    if check ans then return ans else reportError msg
