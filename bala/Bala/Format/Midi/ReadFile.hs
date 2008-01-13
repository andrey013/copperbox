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



-- import Data.Maybe
import Data.Bits
import Data.List (unfoldr)
import Data.Word
import Data.Int
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Numeric


instance Applicative Get where
  pure = return 
  (<*>) = ap



type Result a = Get (Either String a)
success = return . Right
failure = return . Left 

readMidi :: FilePath -> IO (Either String MidiFile)
readMidi path = do
  bs <- L.readFile path
  let ans =  runGet getMidiFile bs
  return ans                  
                   

                 
getMidiFile :: Result MidiFile  
getMidiFile = getHeader `nextWith` \hdr@(Header _ n _) ->
              replicateM (fromIntegral n) getTrack >>= \ts ->
              either fk (sk hdr) (catEithers ts)
  where fk err = return $ Left err
        sk hdr xs = return $ Right $ MidiFile hdr xs


getHeader :: Result Header  
getHeader = assertChars "MThd" `nextSkip` assertSix `nextSkip` header
  where header = getFormat `nextWith` \fmt ->
                 getWord16be >>= \nm ->
                 getTimeDivision `nextWith` \td ->
                 success $ Header fmt nm td
                                          
                                          
        assertSix = getWord32be >>= \i -> return $
                    if (i==6) then (Right 6) 
                              else (Left "header incorrect length value")

getFormat :: Result HFormat
getFormat = fmt <$> getWord16be
  where fmt 0 = Right MF0
        fmt 1 = Right MF1
        fmt 2 = Right MF2
        fmt z = Left $ "getFormat - unrecognized file format " ++ show z




getTimeDivision :: Result TimeDivision
getTimeDivision = division <$> getWord16be
  where division i | i `testBit` 15 = Right $ FPS (i `clearBit` 15)
                   | otherwise      = Right $ TPB i
    
getTrack :: Result Track
getTrack =  assertChars "MTrk" `nextSkip` track
  where track = getWord32be >>= \i ->
                remaining >>= \j ->
                extractTrack (fromIntegral i) j
        extractTrack i j | i <= j = do bs <- getLazyByteString (fromIntegral i)
                                       let evts = runGet getMessages bs
                                       either failure (success . Track) evts
                         | otherwise = failure $ 
                              "getTrack - not enough data remaining - expecting "
                              ++ show i ++ " remaining " ++ show j



           
getMessages :: Result [Message]
getMessages = recGetEvts []
  where
    recGetEvts acc = do
      end <- isEmpty
      if end then (return (catEithers (reverse acc)))
             else (getMessage >>= \e -> recGetEvts (e:acc))


                     
getMessage :: Result Message
getMessage = getVarlen >>= \dt ->
             getWord8 >>= \i ->
             f dt i
  where  
    f dt i  | i == 0xFF               = f2 <$> pure dt <*> getMetaEvent 
            | i >= 0x80 && i < 0xF0   = f2 <$> pure dt <*> getVoiceEvent (split i)
            | i >= 0xF0 && i <= 0xFF  = f2 <$> pure dt <*> getSystemEvent i
            | otherwise               = pure (Left $ "unrecognized message " ++ show i)
    f2 a = applyER ((,) a)
 
    split :: Word8 -> (Word8,Word8) 
    split i = (i .&. 0xF0, i .&. 0x0F)



vEvt fn a = Right $ VoiceEvent $ fn a
vEvt2 fn a b = Right $ VoiceEvent $ fn a b
vEvt3 fn a b c = Right $ VoiceEvent $ fn a b c



getVoiceEvent :: (Word8,Word8) -> Result Event
getVoiceEvent (0x80,ch) = vEvt2 (NoteOff ch)        <$> getWord8 <*> getWord8
getVoiceEvent (0x90,ch) = vEvt2 (NoteOn ch)         <$> getWord8 <*> getWord8
getVoiceEvent (0xA0,ch) = vEvt2 (NoteAftertouch ch) <$> getWord8 <*> getWord8
getVoiceEvent (0xB0,ch) = vEvt2 (Controller ch)     <$> getWord8 <*> getWord8
getVoiceEvent (0xC0,ch) = vEvt  (ProgramChange ch)  <$> getWord8 
getVoiceEvent (0xD0,ch) = vEvt  (ChanAftertouch ch) <$> getWord8 
  



getMetaEvent :: Result Event
getMetaEvent = applyER <$> pure MetaEvent <*> (getWord8 >>= metaEvent)

metaEvent :: Word8 -> Result MetaEvent
metaEvent 0x01 = textEvent <$> pure GENERIC_TEXT      <*> getText
metaEvent 0x02 = textEvent <$> pure COPYRIGHT_NOTICE <*> getText
metaEvent 0x03 = textEvent <$> pure SEQUENCE_NAME     <*> getText
metaEvent 0x04 = textEvent <$> pure INSTRUMENT_NAME   <*> getText
metaEvent 0x05 = textEvent <$> pure LYRICS            <*> getText
metaEvent 0x06 = textEvent <$> pure MARKER            <*> getText
metaEvent 0x07 = textEvent <$> pure CUE_POINT         <*> getText

metaEvent 0x2F = assertWord8 0 `nextSkip` success EndOfTrack

metaEvent 0x51 = assertWord8 3 `nextSkip` setTempo
  where setTempo = getWord24be >>= success . SetTempo

metaEvent 0x54 = assertWord8 5 `nextSkip` smpteOffset
  where smpteOffset = val >>= success 
        val = SMPTEOffset <$> getWord8 <*> getWord8 <*> getWord8 
                                       <*> getWord8 <*> getWord8
                                  
metaEvent 0x58 = assertWord8 4 `nextSkip` timeSig
  where timeSig = val >>= success
        val = TimeSignature <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 
    
metaEvent 0x59 = assertWord8 2 `nextSkip` keySig 
  where keySig = val >>= success 
        val = cnstr <$> getWord8 <*> getWord8
        cnstr ky sc = KeySignature (unwrapint ky) (undscale sc)

metaEvent 0x7F = getVarlen >>=  \i -> 
                 guardedGetStrict (fromIntegral i) `nextWith` \body ->
                 success (SSME i body)

metaEvent er   = error $ "unreconized meta-event " ++ show er
                 
                 
{-  
-- obsolete (FF 20 01 cc) 
-- http://www.borg.com/~jglatt/tech/midifile/obsolete.htm
getMetaEvent 0x20 = do
  failure "here"  
-}


getSystemEvent :: Word8 -> Result Event
getSystemEvent 0xF0 = getVarlen >>=  \i -> 
                 guardedGetStrict (fromIntegral i) `nextWith` \body ->
                 success (SystemEvent (SysEx i body))


  
textEvent :: TextType -> Either String String -> Either String MetaEvent
textEvent ty = applyER (TextEvent ty)

             


getText :: Result String  
getText = getVarlen >>= \i ->  
          guardedGetLazy (fromIntegral i) `nextWith` \bs ->
          success (L.unpack bs)
    

    



  
--------------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------------

nextSkip :: Monad m => m (Either a b) -> m (Either a c) -> m (Either a c)
nextSkip a b = a >>= \ a' -> 
               case a' of 
                 Left l -> return (Left l)
                 Right _ -> b

nextWith :: Monad m => m (Either a b) ->  (b -> m (Either a c)) -> m (Either a c)
nextWith a f = a >>= \ a' -> 
               case a' of 
                 Left l -> return (Left l)
                 Right r -> f r

applyER :: (r -> a) -> Either l r -> Either l a  
applyER f (Left l)  = Left l
applyER f (Right a) = Right (f a)


                 
{-                 
nextER :: Either l a -> Either l b -> Either l b
nextER (Left l)  _   = Left l
nextER (Right _) b   = b


replaceER :: a -> Either l r -> Either l a
replaceER a (Left l)  = Left l
replaceER a (Right _) = Right a

replaceERM :: Monad m => a -> m (Either l r) -> m (Either l a)
replaceERM a b  = b >>= fn 
  where fn (Left l)  = return $ Left l
        fn (Right _) = return $ Right a

applyERM :: Monad m => (r -> a) -> m (Either l r) -> m (Either l a)
applyERM f a  = a >>= fn
  where fn (Left l)  = return $ Left l
        fn (Right a) = return $ Right (f a)

-}






catEithers :: [Either a b] -> Either a [b]
catEithers ls = next ls []
  where next ((Left err):_) acc = Left err
        next ((Right a):xs) acc = next xs (a:acc)
        next [] acc             = Right (reverse acc)
        
guardedGetLazy :: Int64 -> Result L.ByteString
guardedGetLazy i = do
  j <- remaining 
  if (i <= j)
    then (getLazyByteString i >>= success)
    else (failure "guardedGetLazy - not enough data remaining")

guardedGetStrict :: Int -> Result ByteString
guardedGetStrict i = do
  j <- remaining 
  if (fromIntegral i <= j)
    then (getByteString i >>= success)
    else (failure "guardedGetStrict - not enough data remaining")

getTwoParams :: Get (Word8,Word8)
getTwoParams = do
  a <- getWord8
  b <- getWord8
  return (a,b)

getThreeParams :: Get (Word8,Word8,Word8)
getThreeParams = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  return (a,b,c)
  
  
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
assertWord8 :: Word8 -> Result Word8
assertWord8 i = fn <$> getWord8
  where fn j | j == i    = Right i
             | otherwise = Left "assertWord8 failed"
             


assertChars :: String -> Result String
assertChars s = let i = fromIntegral $ length s in fn <$> getLazyByteString i
  where fn bs = case (s == L.unpack bs) of
                  True -> Right s
                  False -> Left "assertChars failed"
                  
    