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
import Data.Char
import Data.Int
import Data.Sequence ( Seq , (|>) )
import qualified Data.Sequence as S
import Data.Word

import System.IO


readMidi :: FilePath -> IO MidiFile  
readMidi filename = either error return =<< runParser midiFile filename
    

--------------------------------------------------------------------------------
-- 
    
    
midiFile :: Parser MidiFile  
midiFile = do
    h  <- header
    se <- countS (trackCount h) track
    return $ MidiFile h se
  where
    trackCount :: Header -> Int 
    trackCount (Header _ n _) = fromIntegral n

header :: Parser Header  
header = Header <$> (assertString "MThd"  *> assertLength (6::Int) *> format)
                <*> getWord16be          <*> timeDivision 

countS :: Int -> Parser a -> Parser (Seq a)
countS i p = step S.empty i
  where 
    step se n   | n <= 0      = return se
                | otherwise   = p >>= \a -> step (se |> a) (n-1)


track :: Parser Track
track = Track <$> 
    (assertString "MTrk" *> getWord32be *> getMessages) 
    

getMessages :: Parser (Seq Message)
getMessages = rec S.empty
  where
    rec acc = do
        end <- eof
        if end then reportFail $ "no end-of-track message before end of input"
               else do step1 acc
    
    step1 acc = do
        msg <- message
        if eot msg then return $ acc |> msg
                   else rec (acc |> msg)

    eot (_, (MetaEvent EndOfTrack)) = True
    eot _                           = False


message :: Parser Message
message = do 
    dt          <- deltaTime 
    (code,chan) <- getWord8split         
    evt         <- next code chan 
    return $ (dt,evt)
  where  
    next code chan  
        | code == 0xF && chan == 0xF  = MetaEvent   <$> (getWord8 >>= metaEvent)         
        | code == 0xF                 = SystemEvent <$> systemEvent
        | code >= 0x8 && code <  0xF  = VoiceEvent  <$> voiceEvent code chan
        | otherwise                   = reportFail $  
            "unrecognized message " ++ hexStr ((code `shiftL` 4) + chan)



deltaTime :: Parser Word32
deltaTime = getVarlen
   
voiceEvent :: Word8 -> Word8 -> Parser VoiceEvent
voiceEvent 0x8 ch = (NoteOff ch)        <$> getWord8 <*> getWord8
voiceEvent 0x9 ch = (NoteOn ch)         <$> getWord8 <*> getWord8
voiceEvent 0xA ch = (NoteAftertouch ch) <$> getWord8 <*> getWord8
voiceEvent 0xB ch = (Controller ch)     <$> getWord8 <*> getWord8
voiceEvent 0xC ch = (ProgramChange ch)  <$> getWord8 
voiceEvent 0xD ch = (ChanAftertouch ch) <$> getWord8 
voiceEvent z   _  = reportFail $ "voiceEvent " ++ hexStr z 




metaEvent :: Word8 -> Parser MetaEvent
metaEvent 0x00 = SequenceNumber   <$> (assertWord8 2     *> getWord16be)
metaEvent 0x01 = textEvent GENERIC_TEXT 
metaEvent 0x02 = textEvent COPYRIGHT_NOTICE
metaEvent 0x03 = textEvent SEQUENCE_NAME
metaEvent 0x04 = textEvent INSTRUMENT_NAME
metaEvent 0x05 = textEvent LYRICS
metaEvent 0x06 = textEvent MARKER
metaEvent 0x07 = textEvent CUE_POINT
metaEvent 0x2F = EndOfTrack       <$   assertWord8 0  
metaEvent 0x51 = SetTempo         <$> (assertWord8 3     *> getWord24be)
metaEvent 0x54 = SMPTEOffset      <$> (assertWord8 5     *> getWord8) 
                                  <*> getWord8          <*> getWord8
                                  <*> getWord8          <*> getWord8
metaEvent 0x58 = TimeSignature    <$> (assertWord8 4     *> getWord8)
                                  <*> getWord8          <*> getWord8 
                                  <*> getWord8 
metaEvent 0x59 = KeySignature     <$> (assertWord8 2     *> getInt8) 
                                  <*> scale
metaEvent 0x7F = (uncurry SSME)   <$> getVarlenBytes     
metaEvent z    = reportFail $ "unreconized meta-event " ++ hexStr z


systemEvent :: Parser SystemEvent
systemEvent = (uncurry SysEx) <$> getVarlenBytes
                      
                          
format :: Parser HFormat
format = getWord16be >>= fn
  where 
    fn 0 = return MF0
    fn 1 = return MF1
    fn 2 = return MF2
    fn z = reportFail $ 
              "getFormat - unrecognized file format " ++ hexStr z
        
timeDivision :: Parser TimeDivision
timeDivision = division <$> getWord16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i


scale :: Parser ScaleType
scale = getWord8 >>= fn
  where
    fn 0 = return MAJOR
    fn 1 = return MINOR
    fn z = reportFail $ "scale expecting 0 or 1, got " ++ hexStr z
    
    
textEvent :: TextType -> Parser MetaEvent
textEvent ty = (TextEvent ty . snd) <$> getVarlenText

--------------------------------------------------------------------------------
-- helpers

getWord24be   :: Parser Word32
getWord24be   = w32be 0 <$> getWord8 <*> getWord8 <*> getWord8


getWord8split :: Parser (Word8,Word8) 
getWord8split = f <$> getWord8 
  where
    f i = ((i .&. 0xF0) `shiftR` 4, i .&. 0x0F)

  


assertWord8 :: Word8 -> Parser Word8
assertWord8 i = getWord8 >>= fn
  where fn j | j == i    = return i
             | otherwise = reportFail $ "assertWord8 failed"
             
assertLength :: Integral a => a -> Parser Word32
assertLength i  = getWord32be >>= fn
  where 
    fn n | n == fromIntegral i  = return n
         | otherwise            = reportFail $
              "assertLength " ++ show i ++ " /= " ++ show n

assertString :: String -> Parser String
assertString s  = getChars (length s) >>= fn
  where 
    fn ss | ss == s   = return s
          | otherwise = reportFail $ 
               "assertString " ++ (showString s []) ++ " /= " ++ (showString ss [])
                                     

getChars :: Int -> Parser String
getChars = count `flip` char

getVarlenText :: Parser (Word32,String)  
getVarlenText = getVarlen                 >>= \i  -> 
                getChars (fromIntegral i) >>= \cs -> return (i,cs)

getVarlenBytes :: Parser (Word32,[Word8]) 
getVarlenBytes = getVarlen    >>= \i  ->  
                 getBytes i   >>= \bs -> return (i,bs)
                 
getVarlen :: Parser Word32
getVarlen = recVarlen 0            
  where
    recVarlen acc = do
        i <- getWord8
        if (varBitHigh i == False)
          then (return $ merge acc i)
          else (recVarlen $ merge acc i)
    
    varBitHigh i = i `testBit` 7
        
    merge acc i = (acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F)
    