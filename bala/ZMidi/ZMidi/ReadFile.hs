
--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.ReadFileAlt
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- An MIDI file parser that using Parsec. 
--
--------------------------------------------------------------------------------

module ZMidi.ReadFile (
    -- * Read a MIDI file
    -- $readmididoc
    readMidi
  ) where

import ZMidi.Datatypes

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char (ord,chr)
import Data.Int
import Data.Monoid
import Data.Sequence hiding (length)
import Data.Word
import System.IO
import Text.ParserCombinators.Parsec hiding (Parser, anyChar)

type WParser a = GenParser Word8 () a

-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
  

-- $readmididoc
-- Read a MIDI file with a Parsec based parser. Seems to work better on Windows
-- than the Data.Binary version.



readMidi :: FilePath -> IO MidiFile  
readMidi name = do 
    h <- openBinaryFile name ReadMode
    bs <- B.hGetContents h
    ans <- parseIO midiFile name (unbytestring bs)    
    case ans of 
      Left err -> hClose h >> error (show err)
      Right mf -> hClose h >> return mf
  where
    unbytestring :: B.ByteString -> [Word8]
    unbytestring = B.foldr (:) [] 
  
parseIO :: WParser a -> FilePath -> [Word8] -> IO (Either ParseError a)
parseIO p name cs = let ans = runParser p () name cs in return ans


midiFile :: WParser MidiFile  
midiFile = 
    header                        >>= \h  -> 
    iters (trackCount h) track    >>= \se -> return $ MidiFile h se
  where
    trackCount :: Header -> Int 
    trackCount (Header _ n _) = fromIntegral n

header :: WParser Header  
header = Header <$> (assertString "MThd"  *> assertLength 6   *> format)
                <*> getWord16be          <*> timeDivision 


iters :: Int -> WParser a -> WParser (Seq a)
iters i p = step mempty i
  where 
    step se i   | i <= 0      = return se
                | otherwise   = p >>= \a -> step (se |> a) (i-1)

track :: WParser Track
track = Track <$> (assertString "MTrk" *> getWord32be *> getMessages) 


getMessages :: WParser (Seq Message)
getMessages = rec mempty
  where
    rec acc = do
        end <- endOfFile
        if end then fail $ "no end-of-track message before end of input"
               else do step1 acc
    
    step1 acc = do
        msg <- message
        if eot msg then return $ acc |> msg
                   else rec (acc |> msg)

    eot (_, (MetaEvent EndOfTrack)) = True
    eot _                           = False


message :: WParser Message
message = deltaTime       >>= \dt           -> 
          getWord8split   >>= \(code,chan)  ->         
          next code chan  >>= \evt          -> return $ (dt,evt)
  where  
    next code chan  
        | code == 0xF && chan == 0xF  = MetaEvent   <$> (getWord8 >>= metaEvent)         
        | code == 0xF                 = SystemEvent <$> systemEvent
        | code >= 0x8 && code <  0xF  = VoiceEvent  <$> voiceEvent code chan
        | otherwise                   = fail $ 
            "unrecognized message " ++ hexStr ((code `shiftL` 4) + chan)

deltaTime :: WParser Word32
deltaTime = getVarlen
   
voiceEvent :: Word8 -> Word8 -> WParser VoiceEvent
voiceEvent 0x8 ch = (NoteOff ch)        <$> getWord8 <*> getWord8
voiceEvent 0x9 ch = (NoteOn ch)         <$> getWord8 <*> getWord8
voiceEvent 0xA ch = (NoteAftertouch ch) <$> getWord8 <*> getWord8
voiceEvent 0xB ch = (Controller ch)     <$> getWord8 <*> getWord8
voiceEvent 0xC ch = (ProgramChange ch)  <$> getWord8 
voiceEvent 0xD ch = (ChanAftertouch ch) <$> getWord8 
voiceEvent z   ch = fail $ "voiceEvent " ++ hexStr z 




metaEvent :: Word8 -> WParser MetaEvent
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
metaEvent z    = fail $ "unreconized meta-event " ++ hexStr z


systemEvent :: WParser SystemEvent
systemEvent = (uncurry SysEx) <$> getVarlenBytes
                      
                          
format :: WParser HFormat
format = getWord16be >>= fn
  where 
    fn 0 = return MF0
    fn 1 = return MF1
    fn 2 = return MF2
    fn z = fail $ "getFormat - unrecognized file format " ++ hexStr z
        
timeDivision :: WParser TimeDivision
timeDivision = division <$> getWord16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i
                   
                   
        
scale :: WParser ScaleType
scale = getWord8 >>= fn
  where
    fn 0 = return MAJOR
    fn 1 = return MINOR
    fn z = fail $ "scale expecting 0 or 1, got " ++ hexStr z



textEvent :: TextType -> WParser MetaEvent
textEvent ty = (TextEvent ty . snd) <$> getVarlenText


--------------------------------------------------------------------------------
-- Helpers 

endOfFile :: WParser Bool
endOfFile = option False (True <$ eof)

getVarlenText :: WParser (Word32,String)  
getVarlenText = getVarlen     >>= \i  -> 
                getChars i    >>= \cs -> return (i,cs)

getVarlenBytes :: WParser (Word32,[Word8]) 
getVarlenBytes = getVarlen    >>= \i  ->  
                 getBytes i   >>= \bs -> return (i,bs)
  
getChars :: Integral a => a -> WParser String  
getChars i = map (chr . fromIntegral) <$> count (fromIntegral i) anyToken          

getBytes :: Integral a => a -> WParser [Word8]
getBytes i = count (fromIntegral i) getWord8

getVarlen :: WParser Word32
getVarlen = recVarlen 0            
  where
    recVarlen acc = do
        i <- getWord8
        if (varBitHigh i == False)
          then (return $ merge acc i)
          else (recVarlen $ merge acc i)
    
    varBitHigh i = i `testBit` 7
        
    merge acc i = (acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F)





getInt8 :: WParser Int8
getInt8 = (fromIntegral . unwrap) <$> getWord8
  where
    unwrap :: Word8 -> Int
    unwrap i | i > 128   = (fromIntegral i) - 256
             | otherwise = fromIntegral i


getWord8      :: WParser Word8
getWord8      = anyToken
           
getWord16be   :: WParser Word16
getWord16be   = w16be     <$> getWord8 <*> getWord8  

getWord32be   :: WParser Word32
getWord32be   = w32be     <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8


getWord24be   :: WParser Word32
getWord24be   = w32be 0   <$> getWord8 <*> getWord8 <*> getWord8


getWord8split :: WParser (Word8,Word8) 
getWord8split = f <$> getWord8 
  where
    f i = ((i .&. 0xF0) `shiftR` 4, i .&. 0x0F)

  

w16be :: Word8 -> Word8 -> Word16
w16be a b = let a' = a `iShiftL` 8
                b' = fromIntegral b 
            in a' + b'  
            
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d = let a' = a `iShiftL` 24
                    b' = b `iShiftL` 16
                    c' = c `iShiftL` 8
                    d' = fromIntegral d
            in a' + b' + c' + d'        

iShiftL :: (Bits b, Integral a, Integral b) => a -> Int -> b
a `iShiftL` x = (fromIntegral a) `shiftL` x

assertWord8 :: Word8 -> WParser Word8
assertWord8 i = getWord8 >>= fn
  where fn j | j == i    = return i
             | otherwise = fail $ "assertWord8 failed"
             
assertLength :: Integral a => a -> WParser Word32
assertLength i  = getWord32be >>= fn
  where 
    fn n | n == fromIntegral i  = return n
         | otherwise            = fail $ 
              "assertLength " ++ show i ++ " /= " ++ show n

assertString :: String -> WParser String
assertString s  = getChars (length s) >>= fn
  where 
    fn ss | ss == s   = return s
          | otherwise = fail $ "assertString " ++ (showString s []) 
                                     ++ " /= " ++ (showString ss [])

