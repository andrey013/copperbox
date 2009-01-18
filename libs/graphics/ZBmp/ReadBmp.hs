{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ReadBmp where

import Types

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char ( chr )
import Data.Word

import System.IO

data ParseState = PSt { remaining :: B.ByteString,
                        pos       :: Int }

newtype ParseErr = ParseErr String
  deriving (Show) 

instance Error ParseErr where
  noMsg = ParseErr ""
  strMsg s = ParseErr s   
                          
newtype Parser a = Parser { 
          getParser :: ErrorT ParseErr (State ParseState) a }
    deriving ( Functor, Monad, MonadError ParseErr, MonadState ParseState ) 

instance Applicative Parser where
  pure = return
  (<*>) = ap
  
runParser :: Parser a -> B.ByteString  -> Either String a   
runParser parser bs0 = case runP parser bs0 of
    Left (ParseErr err) -> Left err
    Right a             -> Right a  
  where 
    runP :: Parser a -> B.ByteString  -> Either ParseErr a
    -- should we use runState and check input is exhausted? 
    runP p bs = evalState (runErrorT $ getParser p) (initState bs)
    
    initState bs = PSt { remaining = bs, pos = 0 }
    
readBmp :: FilePath -> IO BMPfile  
readBmp name = do 
    h <- openBinaryFile name ReadMode
    bs <- B.hGetContents h
    let ans = runParser bmpFile bs    
    case ans of 
      Left err -> hClose h >> putStrLn err >> error "readBmp failed"
      Right mf -> hClose h >> return mf
      
--------------------------------------------------------------------------------

reportFail :: String -> Parser a
reportFail s = do 
    posn <- gets pos
    throwError $ strMsg $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   
    
bmpFile :: Parser BMPfile  
bmpFile = do
    hdr     <- header
    dhdr    <- dibheader
    let w = _dib_width dhdr
    let h = _dib_height dhdr
    body    <- imageData24 w h
    return $ BMPfile hdr dhdr body

header :: Parser BMPheader
header = (\c1 c2 sz r1 r2 off -> BMPheader [c1,c2] sz r1 r2 off )
  <$> char        <*> char        <*> getWord32le 
  <*> getWord16le <*> getWord16le <*> getWord32le 


dibheader :: Parser DIBheader
dibheader = DIBheader 
    <$> getWord32le  <*> getWord32le  <*> getWord32le 
    <*> getWord16le  <*> getWord16le  <*> compression
    <*> getWord32le  <*> getWord32le  <*> getWord32le
    <*> getWord32le  <*> getWord32le

compression :: Parser Compression
compression = unmarshalCompression <$> getWord32le

imageData24 :: Word32 -> Word32 -> Parser [[RGBcolour]]
imageData24 w h = iter h (fst <$> dataLine w)

dataLine :: Word32 -> Parser ([RGBcolour],[Word8])
dataLine w = (,) <$> payload w <*> padding w
  where
    payload i = iter i rgbColour
    padding w = let i = lim $ 4 `mod` (3 * w) in iter i getWord8
    
    lim i | i > 0     = 4 - i
          | otherwise = 0
          

rgbColour :: Parser RGBcolour
rgbColour = RGBcolour <$> getWord8 <*> getWord8 <*> getWord8



--------------------------------------------------------------------------------
-- Helpers

iter :: Integral i => i -> Parser a -> Parser [a]
iter i p | i > 0     = (:) <$> p <*> iter (i-1) p
         | otherwise = return []
         
         
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
                         
char :: Parser Char
char = chr . fromIntegral <$> getWord8

         
assertChar :: Char -> Parser Char
assertChar ch = char >>= fn
  where fn a | a == ch    = return ch
             | otherwise  = reportFail $ "assertChar failed"

getWord16le   :: Parser Word16
getWord16le   = w16le <$> getWord8 <*> getWord8

getWord32le   :: Parser Word32
getWord32le   = w32le <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8


w16le :: Word8 -> Word8 -> Word16
w16le a b = a' + b' where
    a' = fromIntegral a
    b' = b `iShiftL` 8
             
w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = a' + b' + c' + d' where
    a' = fromIntegral a
    b' = b `iShiftL` 8
    c' = c `iShiftL` 16
    d' = d `iShiftL` 24
               

iShiftL :: (Bits b, Integral a, Integral b) => a -> Int -> b
a `iShiftL` x = (fromIntegral a) `shiftL` x

          