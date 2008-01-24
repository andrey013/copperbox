{-# OPTIONS_GHC -fglasgow-exts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Midi.ReadFile
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parse MIDI files 
-- |
--------------------------------------------------------------------------------

-- TODO: add exception handling...

-- note applicative is portable...

-- http://www.sonicspot.com/guide/midifiles.html

module Bala.Format.Midi.ReadFile (
    -- * Read a Midi structure from file
    readMidi
  ) where

import Bala.Format.Midi.Datatypes

import Control.Applicative
import Control.Monad
import Control.Monad.Error


import Data.Bits
import Data.Word
import Data.Int
import qualified Data.Binary.Get as BG
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Numeric

instance Applicative BG.Get where
  pure = return 
  (<*>) = ap
    
instance Applicative (ErrorT ErrorMsg BG.Get) where
  pure = lift . pure
  (<*>) = ap  

  
type ErrorMsg = String

type Parse a = ErrorT ErrorMsg BG.Get a



runParse p bs = BG.runGet (runErrorT p) bs               

readMidi :: FilePath -> IO (Either ErrorMsg MidiFile)
readMidi path = do
  bs <- L.readFile path
  return $ runParse getMidiFile bs
  

getMidiFile :: Parse MidiFile  
getMidiFile = (uncurry MidiFile) <$> contents
  where contents = do
          hdr@(Header _ n _) <- getHeader 
          ts <- replicateM (fromIntegral n) getTrack
          return (hdr,ts)
          
          -- liftA2 ?

     

                                          

getHeader :: Parse Header  
getHeader = Header <$> (assertString "MThd"  *> assertDigit6      *> getFormat)
                   <*> getWord16be          <*> getTimeDivision 



assertDigit6 = f =<< getWord32be
  where 
    f 6 = return 6
    f _ = throwError $ "header incorrect length value"
    
    
getFormat :: Parse HFormat
getFormat = fmt =<< getWord16be
  where fmt 0 = return MF0
        fmt 1 = return MF1
        fmt 2 = return MF2
        fmt z = throwError $ "getFormat - unrecognized file format " ++ show z


getTimeDivision :: Parse TimeDivision
getTimeDivision = division <$> getWord16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i
                   
                       
getTrack :: Parse Track
getTrack = assertString "MTrk" >>
           getWord32be         >>= \i ->
           remaining           >>= \j ->
           extractTrack (fromIntegral i) j
  where           
    extractTrack i j | i <= j = do bs <- getLazyByteString (fromIntegral i)
                                   let ans = runParse getMessages bs
                                   case ans of
                                      Left err -> throwError err
                                      Right ms -> return $ Track ms
  
                     | otherwise = throwError $ 
                                "getTrack - not enough data remaining - expecting "
                                ++ show i ++ " remaining " ++ show j
       
             
getMessages :: Parse [Message]
getMessages = recGetEvts []
  where
    recGetEvts acc = do
      end <- lift BG.isEmpty
      if end then (return (reverse acc))
             else (getMessage >>= \e -> recGetEvts (e:acc))



getMessage :: Parse Message
getMessage = getVarlen >>= \dt ->
             getWord8  >>= \i ->
             f dt i
  where  
    f dt i  | i == 0xFF               = (,) <$> pure dt <*> getMetaEvent 
            | i >= 0x80 && i < 0xF0   = (,) <$> pure dt <*> getVoiceEvent (w8split i)
            | i >= 0xF0 && i <= 0xFF  = (,) <$> pure dt <*> getSystemEvent i
            | otherwise               = throwError $ "unrecognized message " ++ show i

    
     
w8split :: Word8 -> (Word8,Word8) 
w8split i = (i .&. 0xF0, i .&. 0x0F)


ve1 fn a = VoiceEvent $ fn a
ve2 fn a b = VoiceEvent $ fn a b
ve3 fn a b c = VoiceEvent $ fn a b c

getVoiceEvent :: (Word8,Word8) -> Parse Event
getVoiceEvent (0x80,ch) = (ve3 NoteOff ch)        <$> getWord8 <*> getWord8
getVoiceEvent (0x90,ch) = (ve3 NoteOn ch)         <$> getWord8 <*> getWord8
getVoiceEvent (0xA0,ch) = (ve3 NoteAftertouch ch) <$> getWord8 <*> getWord8
getVoiceEvent (0xB0,ch) = (ve3 Controller ch)     <$> getWord8 <*> getWord8
getVoiceEvent (0xC0,ch) = (ve2 ProgramChange ch)  <$> getWord8 
getVoiceEvent (0xD0,ch) = (ve2 ChanAftertouch ch) <$> getWord8 
 
  
    
getMetaEvent :: Parse Event
getMetaEvent = MetaEvent <$> (getWord8 >>= metaEvent)

metaEvent  :: Word8 -> Parse MetaEvent 
metaEvent 0x01 = textEvent GENERIC_TEXT
metaEvent 0x02 = textEvent COPYRIGHT_NOTICE
metaEvent 0x03 = textEvent SEQUENCE_NAME
metaEvent 0x04 = textEvent INSTRUMENT_NAME
metaEvent 0x05 = textEvent LYRICS
metaEvent 0x06 = textEvent MARKER
metaEvent 0x07 = textEvent CUE_POINT

metaEvent 0x2F = EndOfTrack <$ assertWord8 0

metaEvent 0x51 = SetTempo <$> (assertWord8 3  *> getWord24be)
    
metaEvent 0x54 = SMPTEOffset <$> (assertWord8 5  *> getWord8) 
                             <*> getWord8       <*> getWord8
                             <*> getWord8       <*> getWord8
                                  
                                  
                                  
metaEvent 0x58 = TimeSignature <$> (assertWord8 4  *> getWord8)
                               <*> getWord8       <*> getWord8 
                               <*> getWord8
    
metaEvent 0x59 = cnstr <$> (assertWord8 2 *> getWord8) <*> getWord8
  where
    cnstr ky sc = KeySignature (unwrapint ky) (undscale sc)
    
    
metaEvent 0x7F = 
  (uncurry SSME) <$> getVarlen `carry` (guardedGetStrict . fromIntegral)

metaEvent er   = throwError $ "unreconized meta-event " ++ show er


-- helpful?                 
pairA :: Applicative f => f a -> f b -> f (a,b) 
pairA = liftA2 (,)                                  
                 
{-  
-- obsolete (FF 20 01 cc) 
-- http://www.borg.com/~jglatt/tech/midifile/obsolete.htm
getMetaEvent 0x20 = do
  failure "here"  
-}


getSystemEvent :: Word8 -> Parse Event
getSystemEvent 0xF0 = 
    fn <$> (getVarlen `carry` (guardedGetStrict . fromIntegral))
  where fn (i,xs) = SystemEvent $ SysEx i xs   

textEvent :: TextType -> Parse MetaEvent
textEvent ty = (TextEvent ty) <$> getText


    
getText :: Parse String  
getText = L.unpack <$> (getVarlen >>= guardedGetLazy . fromIntegral)
    

--------------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------------

carry :: Monad m => m a -> (a -> m b) -> m (a,b)
carry f g = f >>= \a -> g a >>= \b -> return (a,b) 


guardedGetLazy :: Int64 -> Parse L.ByteString
guardedGetLazy i = do
  j <- lift BG.remaining 
  if (i <= j)
    then  lift $ BG.getLazyByteString i
    else  throwError $ "guardedGetLazy - not enough data remaining"




guardedGetStrict :: Int -> Parse ByteString
guardedGetStrict i = do
  j <- lift BG.remaining 
  if (fromIntegral i <= j)
    then lift $ BG.getByteString i
    else throwError $ "guardedGetStrict - not enough data remaining"




getWord24be :: Parse Word32
getWord24be =  fn <$> getWord8 <*> getWord16be
  where 
    fn :: Word8 -> Word16 -> Word32
    fn hi8 lo16 = ((fromIntegral hi8) `shiftL` 8) + (fromIntegral lo16)

  
getVarlen :: Parse Word32
getVarlen = recVarlen 0
  where
    recVarlen acc = do
      i <- getWord8
      case (bitHigh i) of
        True  -> recVarlen ((acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F))
        False -> return ((acc `shiftL` 7) + (fromIntegral i))
    bitHigh i = i `testBit` 7        
        
                
          
unwrapint :: Word8 -> Int8
unwrapint i 
  | i > 128   = (fromIntegral i) - 256
  | otherwise = fromIntegral i
  
undscale :: Word8 -> Scale
undscale 0 = MAJOR
undscale 1 = MINOR
undscale i = error $ "undscale " ++ show i





assertWord8 :: Word8 -> Parse Word8
assertWord8 i = fn =<< getWord8
  where fn j | j == i    = return i
             | otherwise = throwError $  "assertWord8 failed"

assertString :: String -> Parse String
assertString s = fn =<< lift (BG.getLazyByteString $ lengthi64 s) 
  where 
    fn bs = if (s == L.unpack bs)
              then return s
              else throwError $ "assertChars failed"

getWord8 :: Parse Word8
getWord8 = lift BG.getWord8

getWord16be :: Parse Word16
getWord16be = lift BG.getWord16be

getWord32be :: Parse Word32
getWord32be = lift BG.getWord32be

getLazyByteString :: Int64 -> Parse L.ByteString
getLazyByteString = lift . BG.getLazyByteString


remaining :: Parse Int64
remaining = lift BG.remaining
             
lengthi64 :: [a] -> Int64
lengthi64 = fromIntegral . length


                  
    