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


-- http://www.sonicspot.com/guide/midifiles.html

module Bala.Format.Midi.ReadFile 
    -- * Read a Midi structure from file
--    readMidi
   where

import Bala.Format.Midi.Datatypes
import Bala.Format.Midi.TextualMidi

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Writer


import Data.Bits
import Data.Word
import Data.Int
import qualified Data.Binary.Get as BG
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Numeric
import System.IO (openFile, hClose, IOMode(..), hFileSize )

import Data.Char (chr)

instance Applicative BG.Get where
  pure = return 
  (<*>) = ap
    
instance Applicative (ErrorT ErrorMsg BG.Get) where
  pure = lift . pure
  (<*>) = ap  

  
type ErrorMsg = String


newtype Parse a = Parse { parse :: ErrorT ErrorMsg (WriterT ShowS BG.Get) a }
  deriving (Functor, Monad)


  
instance Applicative Parse where
  pure = return
  (<*>) = ap


runParse :: Parse t -> L.ByteString -> (Either ErrorMsg t, ShowS)
runParse (Parse p) bs = BG.runGet (runWriterT $ runErrorT p) bs 


-- the error monad's `throwError`
parseError :: ErrorMsg -> Parse a
parseError = Parse . throwError

-- the writer monad's `tell`
report :: ShowS -> Parse ()
report = Parse . lift . tell 

reportLine :: ShowS -> Parse ()
reportLine f = Parse $ lift $ tell  (f . showNl) 


-- Weirdly it seems lazy bytestrings aren't always reading the full contents 
-- of a file under Windows

--remaining &/or inner parse the problem

readMidi :: FilePath -> IO (Either ErrorMsg MidiFile, String)
readMidi path = do
  bs <- L.readFile path
  let (a,fw) = runParse getMidiFile bs
  return (a,fw [])

readWords :: FilePath -> IO (Either ErrorMsg [Word8], String)
readWords path = do
  bs <- L.readFile path
  let (a,fw) = runParse (manyZ getWord8) bs
  return (a,fw [])
    
manyZ p = rec []
  where rec acc = do
          e <- isEmpty 
          if (e == True) then return (reverse acc)  
                         else p >>= \a -> rec (a:acc)


getMidiFile :: Parse MidiFile  
getMidiFile = (uncurry MidiFile) <$> contents
  where contents = do
          -- i <- remaining
          -- reportLine $ showString $ "File length is " ++ show i
          hdr@(Header _ n _) <- getHeader 
          ts <- replicateM (fromIntegral n) getTrack
          return (hdr,ts)
          
          -- liftA2 ?

     

                                          

getHeader :: Parse Header  
getHeader = Header <$> (assertString "MThd"  *> assertDigit6      *> getFormat)
                   <*> getWord16be          <*> getTimeDivision 

assertDigit6 :: Parse Word32
assertDigit6 = f =<< getWord32be
  where 
    f 6 = return 6
    f _ = parseError $ "header incorrect length value"
    
    
getFormat :: Parse HFormat
getFormat = fmt =<< getWord16be
  where fmt 0 = return MF0
        fmt 1 = return MF1
        fmt 2 = return MF2
        fmt z = parseError $ "getFormat - unrecognized file format " ++ show z


getTimeDivision :: Parse TimeDivision
getTimeDivision = division <$> getWord16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i


-- Having trouble with bytestrings / data.binary on Windows
-- where consuming input fails after around the 175 bytes
-- Re-writing `getTrack` to not use remaining hasn't helped

{-                       
getTrack :: Parse Track
getTrack = assertString "MTrk" >>
           getWord32be         >>= \i ->
           remaining           >>= \j ->
           extractTrack (fromIntegral i) j
  where           
    extractTrack i j | i <= j = do bs <- getLazyByteString (fromIntegral i)
                                   reportLine $ showString $ "taken " ++ show i
                                   let (ans,w) = runParse getMessages bs
                                   report w
                                   case ans of
                                      Left err -> parseError err
                                      Right ms -> return $ Track ms
  
                     | otherwise = parseError $ 
                                "getTrack - not enough data remaining - expecting "
                                ++ show i ++ " remaining " ++ show j
-}

getTrack :: Parse Track
getTrack = Track <$> (assertString "MTrk" *> getWord32be *> getMessages)   
             
getMessages :: Parse [Message]
getMessages = rec []
  where
    rec acc = do
      end <- return False -- end <- isEmpty
      if end then parseError $ "no end-of-track message befor end of input"
             else do step1 acc
    
    step1 acc = do
      e <- getMessage
      if eot e then return $ reverse (e:acc)
               else rec (e:acc)

    eot (_, (MetaEvent EndOfTrack)) = True
    eot _                           = False


getMessage :: Parse Message
getMessage = (reportLine $ showString "message") >> 
             getVarlen >>= \dt ->
             getWord8  >>= \i ->           
             f dt i
  where  
    f dt i  | i == 0xFF               = (,) <$> pure dt <*> getMetaEvent 
            | i >= 0x80 && i < 0xF0   = (,) <$> pure dt <*> getVoiceEvent (w8split i)
            | i >= 0xF0 && i <= 0xFF  = (,) <$> pure dt <*> getSystemEvent i
            | otherwise               = parseError $ "unrecognized message " ++ show i

   
     
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
  (uncurry SSME) <$> getVarlen `carry` (getSlowByteString . fromIntegral)

metaEvent er   = parseError $ "unreconized meta-event " ++ show er


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
    fn <$> (getVarlen `carry` (getSlowByteString . fromIntegral))
  where fn (i,xs) = SystemEvent $ SysEx i xs   

textEvent :: TextType -> Parse MetaEvent
textEvent ty = (TextEvent ty) <$> getText


    
getText :: Parse String  
getText = do i <- getVarlen
             ws <- replicateM (fromIntegral i) getWord8
             return $ packPlainString ws


  

--------------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------------

carry :: Monad m => m a -> (a -> m b) -> m (a,b)
carry f g = f >>= \a -> g a >>= \b -> return (a,b) 


  
-- More changes for Windows bytestring / data binary problem
 
{-
guardedGetLazy :: Int64 -> Parse L.ByteString
guardedGetLazy i = do
  j <- remaining 
  if (i <= j)
    then getLazyByteString i
    else parseError $ "guardedGetLazy - not enough data remaining"




guardedGetStrict :: Int -> Parse ByteString
guardedGetStrict i = do
  j <- remaining 
  if (fromIntegral i <= j)
    then getByteString i
    else parseError $ "guardedGetStrict - not enough data remaining"
-}

getSlowByteString :: Int -> Parse ByteString
getSlowByteString i = pack <$> replicateM (fromIntegral i) getWord8        

getSlowString :: Int -> Parse String
getSlowString i = packPlainString <$> replicateM (fromIntegral i) getWord8        
  

getWord24be :: Parse Word32
getWord24be =  fn <$> getWord8 <*> getWord16be
  where 
    fn :: Word8 -> Word16 -> Word32
    fn hi8 lo16 = ((fromIntegral hi8) `shiftL` 8) + (fromIntegral lo16)


  
getVarlen :: Parse Word32
getVarlen = do a <- recVarlen 0
               reportLine (brepVarlen a)
               return a               
  where
    recVarlen acc = do
      i <- getWord8norep
      case (bitHigh i) of
        True  -> recVarlen $ (acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F)
        False -> return $ (acc `shiftL` 7) + (fromIntegral i)
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
             | otherwise = parseError $  "assertWord8 failed"



assertString :: String -> Parse String
assertString ss = fn =<< (getSlowString $ length ss)
  where 
    fn :: String -> Parse String
    fn xs = if (ss == xs)
              then reportLine (brepStr ss) >> return ss
              else parseError $ "assertChars failed"


s0 = L.pack ""
s12 = L.pack "12"


getBytes :: Int -> Parse ByteString
getBytes = Parse . lift . lift . BG.getBytes

isEmpty :: Parse Bool
isEmpty = Parse $ lift $ lift BG.isEmpty


getWord8norep :: Parse Word8
getWord8norep = Parse $ lift $ lift BG.getWord8

getWord8 :: Parse Word8
getWord8 = getWord8norep >>= \a ->
           reportLine (brep1 shows a) >>
           return a

getWord16be :: Parse Word16
getWord16be = (Parse $ lift $ lift BG.getWord16be) >>= \a ->
              reportLine (brep1 shows a) >>
              return a


getWord32be :: Parse Word32
getWord32be = (Parse $ lift $ lift BG.getWord32be) >>= \a ->
              reportLine (brep1 shows a) >>
              return a

packPlainString :: [Word8] -> String
packPlainString = foldr (\e acc -> f e : acc) [] 
  where f = chr . fromIntegral
  
-- More changes for Windows bytestring / data binary problem
 
{-
getLazyByteString :: Int64 -> Parse L.ByteString
getLazyByteString = Parse . lift . lift . BG.getLazyByteString

getByteString :: Int -> Parse ByteString
getByteString = Parse . lift . lift . BG.getByteString

remaining :: Parse Int64
remaining = Parse $ lift $ lift BG.remaining
             
lengthi64 :: [a] -> Int64
lengthi64 = fromIntegral . length

-}

test = let (a,wf) = runParse pfun input
       in do putStr (wf [])
             putStrLn $ show a
  where build = L.pack . map chr
        input = build $
          [0x1a, 0x89, 0x3d, 0x00, 
           0x82, 0x67, 0x99, 0x3e,
           0x3e, 0x00]
        pfun = (,) <$> getMessage <*> getMessage 


                  
    