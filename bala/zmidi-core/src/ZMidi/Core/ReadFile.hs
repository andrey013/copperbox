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
    readMidi

  -- * Auxiallary types
  , ParseErr
  , Pos
  , ErrMsg
  
  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.ParserMonad


import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy   as L
import Data.Word


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
message = (,) <$>  deltaTime <*> event

deltaTime :: ParserM Word32
deltaTime = getVarlen

event :: ParserM Event
event = word8 >>= step
  where
    -- 00..7f  -- /data/
    step n | n    == 0xFF   = MetaEvent        <$> (word8 >>= metaEvent)
           | 0xF8 <= n      = SysRealTimeEvent <$> sysRealTimeEvent n
           | 0xF1 <= n      = SysCommonEvent   <$> sysCommonEvent n
           | n    == 0xF0   = SysExEvent       <$> sysExEvent
           | 0x80 <= n      = VoiceEvent       <$> voiceEvent (splitByte n)
           | otherwise      = DataEvent        <$> dataEvent n

dataEvent :: Word8 -> ParserM DataEvent
dataEvent tag = (\b c d -> Data4 tag b c d) <$> word8 <*> word8 <*> word8


-- hex2 :: Integral a => a -> String
-- hex2 = ($ "") . showHex 
   
voiceEvent :: SplitByte -> ParserM VoiceEvent
voiceEvent (SB 0x8 ch)  = (NoteOff ch)        <$> word8 <*> word8
voiceEvent (SB 0x9 ch)  = (NoteOn ch)         <$> word8 <*> word8
voiceEvent (SB 0xA ch)  = (NoteAftertouch ch) <$> word8 <*> word8
voiceEvent (SB 0xB ch)  = (Controller ch)     <$> word8 <*> word8
voiceEvent (SB 0xC ch)  = (ProgramChange ch)  <$> word8 
voiceEvent (SB 0xD ch)  = (ChanAftertouch ch) <$> word8 
voiceEvent (SB 0xE ch)  = (PitchBend ch)      <$> word16be
voiceEvent (SB z   _ )  = reportError $ "voiceEvent " ++ hexStr z 


sysCommonEvent :: Word8 -> ParserM SysCommonEvent
sysCommonEvent 0xF1 = QuarterFrame . splitByte      <$> word8
sysCommonEvent 0xF2 = SongPosPointer                <$> word8 <*> word8
sysCommonEvent 0xF3 = SongSelect                    <$> word8
sysCommonEvent 0xF4 = pure $ Common_undefined 0xF4
sysCommonEvent 0xF5 = pure $ Common_undefined 0xF5
sysCommonEvent 0xF6 = pure TuneRequest
sysCommonEvent 0xF7 = pure EOX
sysCommonEvent tag  = pure $ Common_undefined tag


sysRealTimeEvent :: Word8 -> ParserM SysRealTimeEvent
sysRealTimeEvent 0xF8 = pure TimingClock
sysRealTimeEvent 0xF9 = pure $ RT_undefined 0xF9
sysRealTimeEvent 0xFA = pure StartSequence
sysRealTimeEvent 0xFB = pure ContinueSequence
sysRealTimeEvent 0xFC = pure StopSequence
sysRealTimeEvent 0xFD = pure $ RT_undefined 0xFD
sysRealTimeEvent 0xFE = pure ActiveSensing
sysRealTimeEvent 0xFF = pure SystemReset
sysRealTimeEvent tag  = pure $ RT_undefined tag


sysExEvent :: ParserM SysExEvent
sysExEvent = (uncurry SysEx) <$> getVarlenBytes
                      


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
getVarlen = liftM fromVarlen step1
  where
    high a = a `testBit` 7

    step1     = word8 >>= \a -> if high a then step2 a else return (V1 a)
    step2 a   = word8 >>= \b -> if high b then step3 a b else return (V2 a b)
    step3 a b = word8 >>= \c -> if high c then do { d <- word8
                                                  ; return (V4 a b c d)}
                                          else return (V3 a b c)  




-- | Apply parse then apply the check, if the check fails report
-- the error message. 
postCheck :: ParserM a -> (a -> Bool) -> String -> ParserM a
postCheck p check msg = p >>= \ans -> 
    if check ans then return ans else reportError msg
