{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  MidiRead
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  volatile
-- Portability :  to be determined.
--
-- A MIDI file parser. 
--
--------------------------------------------------------------------------------

module MidiRead (
    -- * Read a MIDI file
    -- $readmididoc
    readMidi
  ) where

import MidiDatatypes

import Data.ParserCombinators.Kangaroo

import Control.Applicative

import Data.Bits
import Data.Sequence ( Seq , (<|), (|>) )
import qualified Data.Sequence as S
import Data.Word



readMidi :: FilePath -> IO MidiFile  
readMidi filename = either error return =<< runKangaroo midiFile filename
    

--------------------------------------------------------------------------------
-- 
    
    
midiFile :: Kangaroo MidiFile  
midiFile = do
    h  <- header
    se <- countS (trackCount h) track
    return $ MidiFile h se
  where
    trackCount :: Header -> Int 
    trackCount (Header _ n _) = fromIntegral n

header :: Kangaroo Header  
header = Header <$> (assertString "MThd"  *> assertWord32 (6::Int) *> format)
                <*> word16be             <*> timeDivision 

countS :: Int -> Kangaroo a -> Kangaroo (Seq a)
countS = genericCount (<|) S.empty 


track :: Kangaroo Track
track = Track <$> (assertString "MTrk" *> (word32be >>= getMessages))
--    `substError` "track"

getMessages :: Word32 -> Kangaroo (Seq Message)
{-
getMessages i = withinRegionRelRecto 0 (fromIntegral i) messages
  where    
    messages = genericRunOn (<|) S.empty message

-}
getMessages _ = rec S.empty
  where
    rec acc = do
        end <- atEnd
        if end then reportError $ "no end-of-track message before end of input"
               else do step1 acc
    
    step1 acc = do
        msg <- message
        if isEOT msg then return $ acc |> msg
                     else rec (acc |> msg)



isEOT (_, (MetaEvent EndOfTrack)) = True
isEOT _                           = False


message :: Kangaroo Message
message = do 
    dt          <- deltaTime 
    (code,chan) <- word8split         
    evt         <- next code chan 
    return $ (dt,evt)
  where  
    next code chan  
        | code == 0xF && chan == 0xF  = MetaEvent   <$> (word8 >>= metaEvent)         
        | code == 0xF                 = SystemEvent <$> systemEvent
        | code >= 0x8 && code <  0xF  = VoiceEvent  <$> voiceEvent code chan
        | otherwise                   = reportError $  
            "unrecognized message " ++ hexStr ((code `shiftL` 4) + chan)



deltaTime :: Kangaroo Word32
deltaTime = getVarlen
   
voiceEvent :: Word8 -> Word8 -> Kangaroo VoiceEvent
voiceEvent 0x8 ch = (NoteOff ch)        <$> word8 <*> word8
voiceEvent 0x9 ch = (NoteOn ch)         <$> word8 <*> word8
voiceEvent 0xA ch = (NoteAftertouch ch) <$> word8 <*> word8
voiceEvent 0xB ch = (Controller ch)     <$> word8 <*> word8
voiceEvent 0xC ch = (ProgramChange ch)  <$> word8 
voiceEvent 0xD ch = (ChanAftertouch ch) <$> word8 
voiceEvent z   _  = reportError $ "voiceEvent " ++ hexStr z 




metaEvent :: Word8 -> Kangaroo MetaEvent
metaEvent 0x00 = SequenceNumber   <$> (assertWord8 2     *> word16be)
metaEvent 0x01 = textEvent GENERIC_TEXT 
metaEvent 0x02 = textEvent COPYRIGHT_NOTICE
metaEvent 0x03 = textEvent SEQUENCE_NAME
metaEvent 0x04 = textEvent INSTRUMENT_NAME
metaEvent 0x05 = textEvent LYRICS
metaEvent 0x06 = textEvent MARKER
metaEvent 0x07 = textEvent CUE_POINT
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


systemEvent :: Kangaroo SystemEvent
systemEvent = (uncurry SysEx) <$> getVarlenBytes
                      
                          
format :: Kangaroo HFormat
format = word16be >>= fn
  where 
    fn 0 = return MF0
    fn 1 = return MF1
    fn 2 = return MF2
    fn z = reportError $ 
              "getFormat - unrecognized file format " ++ hexStr z
        
timeDivision :: Kangaroo TimeDivision
timeDivision = division <$> word16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i


scale :: Kangaroo ScaleType
scale = word8 >>= fn
  where
    fn 0 = return MAJOR
    fn 1 = return MINOR
    fn z = reportError $ "scale expecting 0 or 1, got " ++ hexStr z
    
    
textEvent :: TextType -> Kangaroo MetaEvent
textEvent ty = (TextEvent ty . snd) <$> getVarlenText

--------------------------------------------------------------------------------
-- helpers

{-
-- enumerate doesn't really seem worth it for scale etc... 
enumerate :: Integral a => [ans] -> a -> String -> Kangaroo ans
enumerate []     _ msg = reportError msg
enumerate (x:xs) i msg | i == 0    = return x
                       | otherwise = enumerate xs (i-1) msg

-}


word24be   :: Kangaroo Word32
word24be   = w32be 0 <$> word8 <*> word8 <*> word8


word8split :: Kangaroo (Word8,Word8) 
word8split = split <$> word8 
  where
    split i = ((i .&. 0xF0) `shiftR` 4, i .&. 0x0F)

 
assertWord8 :: Word8 -> Kangaroo Word8
assertWord8 i = postCheck word8 (==i) msg
  where 
    msg = "assertWord8 - input did not match " ++ show i
             
assertWord32 :: Integral a => a -> Kangaroo Word32
assertWord32 i = postCheck word32be ((==i) . fromIntegral) msg
  where
    msg = "assertWord32 - input did not match " ++ show i

assertString :: String -> Kangaroo String
assertString s = postCheck (text $ length s) (==s) msg
  where
    msg = "assertString - input did not match " ++ s


getVarlenText :: Kangaroo (Word32,String)  
getVarlenText = countPrefixed getVarlen char

getVarlenBytes :: Kangaroo (Word32,[Word8]) 
getVarlenBytes = countPrefixed getVarlen word8


getVarlen :: Kangaroo Word32
getVarlen = buildWhile (`testBit` 7) merge merge 0 word8
  where
    merge i acc = (acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F)
   


