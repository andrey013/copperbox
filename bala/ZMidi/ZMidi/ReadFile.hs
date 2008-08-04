
--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.ReadFile
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parse MIDI files with Data.Binary.
--
--------------------------------------------------------------------------------



module ZMidi.ReadFile (
    -- * Read a MIDI file
    -- $readmididoc
    readMidi
  ) where

import ZMidi.Datatypes



import Control.Applicative
import Control.Monad
import qualified Data.Binary.Get as BG
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (chr)
import Data.Int
import Data.Monoid
import Data.Sequence hiding (length)
import Data.Word



type Parser a = BG.Get a


runParse :: Parser a -> L.ByteString -> a
runParse = BG.runGet


-- $readmididoc
-- Weirdly it seems lazy bytestrings aren't always reading the full contents
-- of a file under Windows.
--
-- Use ReadFileAlt if you see exceptions due to early end of file.

readMidi :: FilePath -> IO MidiFile
readMidi path = do
    bs <- L.readFile path
    let ans  = runParse midiFile bs
    return ans

midiFile :: Parser MidiFile
midiFile = do
    hdr@(Header _ n _) <- header
    ts <- count (fromIntegral n) track
    return $ MidiFile hdr (fromList ts)


header :: Parser Header
header = Header <$> (assertString "MThd"  *> assertLength 6   *> format)
                <*> getWord16be          <*> timeDivision


track :: Parser Track
track = Track <$> (assertString "MTrk" *> getWord32be *> getMessages)


getMessages :: Parser (Seq Message)
getMessages = rec mempty
  where
    rec acc = do
        end <- endOfFile
        if end then fail $ "no end-of-track message before end of input"
               else do step1 acc

    step1 acc = do
        msg <- message
        if eot msg then return $ acc |> msg
                   else rec $ acc |> msg

    eot (Message (_, (MetaEvent EndOfTrack))) = True
    eot _                                     = False


message :: Parser Message
message = deltaTime      >>=           \dt ->
          getWord8split  >>=  \(code,chan) ->
          next code chan >>=          \evt ->
          return $ Message (dt,evt)
  where
    next code chan
        | code == 0xF && chan == 0xF  = MetaEvent   <$> metaEvent
        | code == 0xF                 = SystemEvent <$> systemEvent
        | code >= 0x8 && code <  0xF  = VoiceEvent  <$> voiceEvent (code,chan)
        | otherwise                   = fail $
            "unrecognized message " ++ hexStr ((code `shiftL` 4) + chan)

deltaTime :: Parser Word32
deltaTime = getVarlen

voiceEvent :: (Word8,Word8) -> Parser VoiceEvent
voiceEvent (code,chan) =
    case code of
      0x8 -> noteOff chan
      0x9 -> noteOn chan
      0xA -> noteAftertouch chan
      0xB -> controller chan
      0xC -> programChange chan
      0xD -> chanAftertouch chan
      z   -> fail $ "voiceEvent " ++ hexStr z


noteOff ch          = (NoteOff ch)        <$> getWord8 <*> getWord8
noteOn ch           = (NoteOn ch)         <$> getWord8 <*> getWord8
noteAftertouch ch   = (NoteAftertouch ch) <$> getWord8 <*> getWord8
controller ch       = (Controller ch)     <$> getWord8 <*> getWord8
programChange ch    = (ProgramChange ch)  <$> getWord8
chanAftertouch ch   = (ChanAftertouch ch) <$> getWord8


metaEvent :: Parser MetaEvent
metaEvent = getWord8 >>= fn
  where
    fn 0x01 = genericText
    fn 0x02 = copyrightNotice
    fn 0x03 = sequenceName
    fn 0x04 = instrumentName
    fn 0x05 = lyrics
    fn 0x06 = marker
    fn 0x07 = cuePoint
    fn 0x2F = endOfTrack
    fn 0x51 = setTempo
    fn 0x54 = smpteOffset
    fn 0x58 = timeSignature
    fn 0x59 = keySignature
    fn 0x7F = ssme
    fn z    = fail $ "unreconized meta-event " ++ hexStr z


genericText     = textEvent GENERIC_TEXT
copyrightNotice = textEvent COPYRIGHT_NOTICE
sequenceName    = textEvent SEQUENCE_NAME
instrumentName  = textEvent INSTRUMENT_NAME
lyrics          = textEvent LYRICS
marker          = textEvent MARKER
cuePoint        = textEvent CUE_POINT


endOfTrack      = EndOfTrack  <$   assertWord8 0

setTempo        = SetTempo    <$> (assertWord8 3  *> getWord24be)

smpteOffset     = SMPTEOffset <$> (assertWord8 5  *> getWord8)
                              <*> getWord8       <*> getWord8
                              <*> getWord8       <*> getWord8

timeSignature   = TimeSignature   <$> (assertWord8 4  *> getWord8)
                                  <*> getWord8       <*> getWord8
                                  <*> getWord8

keySignature    = KeySignature    <$> (assertWord8 2 *> getInt8) <*> scale

ssme =  getVarlen  >>= \i ->
        getBytes i >>= \ws ->
        return (SSME i ws)



systemEvent :: Parser SystemEvent
systemEvent = getVarlen  >>= \i ->
              getBytes i >>= \ws ->
              return (SysEx i ws)


format :: Parser HFormat
format = getWord16be >>= fn
  where
    fn 0 = return MF0
    fn 1 = return MF1
    fn 2 = return MF2
    fn z = fail $ "getFormat - unrecognized file format " ++ hexStr z

timeDivision :: Parser TimeDivision
timeDivision = division <$> getWord16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i



scale :: Parser ScaleType
scale = getWord8 >>= fn
  where
    fn 0 = return MAJOR
    fn 1 = return MINOR
    fn z = fail $ "scale expecting 0 or 1, got " ++ hexStr z



textEvent :: TextType -> Parser MetaEvent
textEvent ty = (TextEvent ty) <$> getText


--------------------------------------------------------------------------------
-- Helpers

count :: Int -> Parser a -> Parser [a]
count = replicateM

anyChar :: Parser Char
anyChar = getWord8 >>= \i -> return (chr $ fromIntegral i)

endOfFile :: Parser Bool
endOfFile = BG.isEmpty

getText :: Parser String
getText = getVarlen >>= \i -> count (fromIntegral i) anyChar

getChars :: Integral a => a -> Parser String
getChars i = count (fromIntegral i) anyChar

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

getBytes :: Integral a => a -> Parser [Word8]
getBytes i = count (fromIntegral i) getWord8



getInt8 :: Parser Int8
getInt8 = unwrap <$> getWord8
  where
    unwrap i | i > 128   = (fromIntegral i) - 256
             | otherwise = fromIntegral i


getWord8 :: Parser Word8
getWord8 = BG.getWord8

getWord16be :: Parser Word16
getWord16be = BG.getWord16be


getWord32be :: Parser Word32
getWord32be = BG.getWord32be

getWord24be :: Parser Word32
getWord24be =  fn <$> getWord8 <*> getWord16be
  where
    fn :: Word8 -> Word16 -> Word32
    fn a b = ((fromIntegral a) `shiftL` 16) + (fromIntegral b)

getWord8split :: Parser (Word8,Word8)
getWord8split = f <$> getWord8
  where
    f i = ((i .&. 0xF0) `shiftR` 4, i .&. 0x0F)




assertWord8 :: Word8 -> Parser Word8
assertWord8 i = getWord8 >>= fn
  where fn j | j == i    = return i
             | otherwise = fail $ "assertWord8 failed"

assertLength :: Integral a => a -> Parser Word32
assertLength i  = getWord32be >>= fn
  where
    fn n | n == fromIntegral i  = return n
         | otherwise            = fail $
              "assertLength " ++ show i ++ " /= " ++ show n

assertString :: String -> Parser String
assertString s  = getChars (length s) >>= fn
  where
    fn ss | ss == s   = return s
          | otherwise = fail $ "assertString " ++ (showString s [])
                                     ++ " /= " ++ (showString ss [])



