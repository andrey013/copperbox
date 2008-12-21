{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

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
-- A MIDI file parser. 
--
--------------------------------------------------------------------------------

module ZMidi.ReadFile (
    -- * Read a MIDI file
    -- $readmididoc
    readMidi
  ) where

import ZMidi.Datatypes
import ZMidi.TextualMidi

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Int
import Data.Sequence hiding (reverse, length)
import Data.Word

import System.IO
import Text.PrettyPrint.HughesPJ

newtype ParseErr = ParseErr String
  deriving (Show) 
  
instance Error ParseErr where
  noMsg = ParseErr ""
  strMsg s = ParseErr s 
  
data ParseState = PSt { remaining :: B.ByteString,
                        pos       :: Int }

newtype Parser a = Parser { 
          getParser :: ErrorT ParseErr (WriterT String (State ParseState)) a }

          
instance Functor Parser where
    fmap f = Parser . fmap f . getParser

instance Monad Parser where
  return a = Parser $ return a
  ma >>= f = Parser $ getParser ma >>= getParser . f
  
instance Applicative Parser where
  pure = return
  (<*>) = ap

instance MonadState ParseState Parser where
  get     = Parser $ get 
  put f   = Parser $ put f
    
instance MonadWriter String Parser where
  tell    = Parser . tell
  listen  = Parser . listen . getParser
  pass    = Parser . pass . getParser 


instance MonadError ParseErr Parser where
  throwError     = Parser . throwError
  catchError m f = Parser $ 
                     catchError (getParser m) (getParser . f)





runParser :: Parser a -> B.ByteString  -> Either String a   
runParser p bs = case runP p bs of
    (Left (ParseErr err), ss) -> Left $ errorstring err ss 
    (Right a, _)              -> Right a  
  where 
    runP :: Parser a -> B.ByteString  -> (Either ParseErr a,String) 
    -- should we use runState and check input is exhausted? 
    runP p bs = evalState (runWriterT (runErrorT $ getParser p)) st0
    
    st0 = PSt { remaining = bs, pos = 0 }
    
    errorstring err ss = err ++ "\n\nParse result upto error...\n" ++ ss 

readMidi :: FilePath -> IO MidiFile  
readMidi name = do 
    h <- openBinaryFile name ReadMode
    bs <- B.hGetContents h
    let ans = runParser midiFile bs    
    case ans of 
      Left err -> hClose h >> putStrLn err >> error "readMidi failed"
      Right mf -> hClose h >> return mf
  where
    unbytestring :: B.ByteString -> [Word8]
    unbytestring = B.foldr (:) [] 
    

--------------------------------------------------------------------------------
-- 

reportFail :: String -> Parser a
reportFail s = do 
    pos <- filePosition
    throwError $ strMsg $ s ++ posStr pos
  where
    posStr pos = " position " ++ show pos   

tellLine :: String -> Parser ()
tellLine s = tell s >> tell "\n" 

tellDoc :: Doc -> Parser ()
tellDoc = tellLine . show  
    
    
midiFile :: Parser MidiFile  
midiFile = do
    h  <- header
    tellDoc (ppHeader h)
    se <- iters (trackCount h) track
    return $ MidiFile h se
  where
    trackCount :: Header -> Int 
    trackCount (Header _ n _) = fromIntegral n

header :: Parser Header  
header = Header <$> (assertString "MThd"  *> assertLength 6   *> format)
                <*> getWord16be          <*> timeDivision 


iters :: Int -> Parser a -> Parser (Seq a)
iters i p = step mempty i
  where 
    step se i   | i <= 0      = return se
                | otherwise   = p >>= \a -> step (se |> a) (i-1)

track :: Parser Track
track = Track <$> 
    (newtrack *> assertString "MTrk" *> getWord32be *> getMessages) 
  where
    newtrack = tellLine "Starting new track..."
    

getMessages :: Parser (Seq Message)
getMessages = rec mempty
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
    tellDoc $ ppMessage (dt,evt)
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
voiceEvent z   ch = reportFail $ "voiceEvent " ++ hexStr z 




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

eof :: Parser Bool
eof = do
     bs <- gets remaining
     return $ B.null bs 
   
getWord8 :: Parser Word8
getWord8 = do
    bs <- gets remaining
    case B.uncons bs of
        Nothing    -> reportFail "Unexpected eof" 
        Just (a,b) -> do { i <- gets pos 
                         ; modify (\s -> s { remaining=b, pos=i+1 } )
                         ; return a } 

filePosition :: Parser Int
filePosition = gets pos

count :: Integral i => i -> Parser a -> Parser [a]
count i p = step i [] where
  step i xs  | i <= 0     = return (reverse xs)
             | otherwise  = p >>= \a -> step (i-1) (a:xs)
             


getInt8 :: Parser Int8
getInt8 = (fromIntegral . unwrap) <$> getWord8
  where
    unwrap :: Word8 -> Int
    unwrap i | i > 128   = (fromIntegral i) - 256
             | otherwise = fromIntegral i

getWord16be   :: Parser Word16
getWord16be   = w16be     <$> getWord8 <*> getWord8  

getWord32be   :: Parser Word32
getWord32be   = w32be     <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8


getWord24be   :: Parser Word32
getWord24be   = w32be 0   <$> getWord8 <*> getWord8 <*> getWord8


getWord8split :: Parser (Word8,Word8) 
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
                                     

getChars :: Integral a => a -> Parser [Char]  
getChars i = map (chr . fromIntegral) <$> count i getWord8 

getBytes :: Integral a => a -> Parser [Word8]
getBytes i = count i getWord8

getVarlenText :: Parser (Word32,String)  
getVarlenText = getVarlen     >>= \i  -> 
                getChars i    >>= \cs -> return (i,cs)

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
    