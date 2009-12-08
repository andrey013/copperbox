{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DLLexports.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A MIDI file parser. 
--
--------------------------------------------------------------------------------

module DLLexports.ParseMonad where

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import System.IO

type ParseErr = String

type ImageData = IOUArray Int Word8   -- Is Int big enough for index?
  
data ParseState = PSt { imagearr  :: ImageData 
                      , pos       :: Int 
                      }

newtype Parser a = Parser { getParser :: ParseState -> IO (ParseState,Either ParseErr a) }
          
instance Functor Parser where
    fmap f (Parser x) = Parser $ \st ->  x st `bindIO` \(st',a) -> return (st',fmap f a)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO = (>>=)

returnIO :: a -> IO a
returnIO = return


instance Monad Parser where
  return a = Parser $ \st -> returnIO (st,Right a)
  (Parser x) >>= f = Parser $ \st -> x st `bindIO` \(st',ans) ->
                                     case ans of 
                                        Left err -> returnIO (st',Left err)
                                        Right a  -> getParser (f a) st'



instance Applicative Parser where
  pure = return
  (<*>) = ap


getSt :: Parser ParseState
getSt = Parser $ \st -> return (st, Right st)

getSt_ :: (ParseState -> a) -> Parser a
getSt_ f = Parser $ \st -> return (st, Right $ f st)

putSt :: ParseState -> Parser ()
putSt st = Parser $ \_ -> return (st, Right ())

modifySt :: (ParseState -> ParseState) -> Parser ()
modifySt f = Parser $ \st -> return (f st, Right ())


throwErr :: String -> Parser a
throwErr msg = Parser $ \st -> return (st,Left msg)

liftIOAction :: IO a -> Parser a
liftIOAction ma = Parser $ \st -> ma >>= \a -> return (st,Right a) 


runParser :: Parser a -> FilePath -> IO (Either ParseErr a)
runParser p filename = withBinaryFile filename ReadMode $ \ handle -> do 
    sz'     <- hFileSize handle
    let sz = fromIntegral sz'
    arr     <- newArray_ (0,sz-1)
    _rsz    <- hGetArray handle arr  (fromIntegral sz)
    (_,ans) <- runP p arr
    return ans   
  where 
    runP :: Parser a -> ImageData -> IO (ParseState, Either ParseErr a) 
    -- should we use runState and check input is exhausted? 
    runP (Parser x) arr = x (initState arr)
    
    initState arr = PSt { imagearr = arr, pos = 0 }
    
    errorstring err ss = err ++ "\n\nParse result upto error...\n" ++ ss 
    


--------------------------------------------------------------------------------
-- 


   
getWord8 :: Parser Word8
getWord8 = do
    PSt ar ix  <- getSt
    a <- liftIOAction $ readArray ar ix
    putSt (PSt ar (ix+1))
    return a


--------------------------------------------------------------------------------
-- helpers





reportFail :: String -> Parser a
reportFail s = do 
    posn <- getSt_ pos
    throwErr $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   


eof :: Parser Bool
eof = do
     PSt ar ix  <- getSt
     (_,up)  <- liftIOAction $ getBounds ar
     return $ (ix>=up) 



getBytes :: Integral a => a -> Parser [Word8]
getBytes i = count (fromIntegral i) getWord8

getChar8bit :: Parser Char
getChar8bit = (chr . fromIntegral) <$> getWord8 

filePosition :: Parser Int
filePosition = getSt_ pos


count :: Int -> Parser a -> Parser [a]
count i p = step i [] where
  step n xs  | n <= 0     = return (reverse xs)
             | otherwise  = p >>= \a -> step (n-1) (a:xs)
             


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

getWord16le   :: Parser Word16
getWord16le   = w16le     <$> getWord8 <*> getWord8  

getWord32le   :: Parser Word32
getWord32le   = w32le     <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8

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

w16le :: Word8 -> Word8 -> Word16
w16le a b = let a' = fromIntegral a 
                b'  = b `iShiftL` 8
            in a' + b'  

w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = let a' = fromIntegral a
                    b' = b `iShiftL` 8
                    c' = c `iShiftL` 16
                    d' = d `iShiftL` 24
            in a' + b' + c' + d'      


iShiftL :: (Bits b, Integral a, Integral b) => a -> Int -> b
a `iShiftL` x = (fromIntegral a) `shiftL` x      




